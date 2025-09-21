;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INVALID-STATUS (err u400))
(define-constant ERR-EXPIRED (err u408))
(define-constant ERR-ALREADY-DISPUTED (err u409))
(define-constant ERR-INSUFFICIENT-AMOUNT (err u402))
(define-constant ERR-INVALID-RATING (err u403))
(define-constant ERR-INVALID-PERCENTAGE (err u405))

;; Define constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ESCROW-FEE u100) ;; 1% fee (100/10000)
(define-constant DISPUTE-TIMEOUT u1440) ;; 24 hours in blocks
(define-constant MAX-RATING u5)

;; Define data vars
(define-data-var admin-address principal CONTRACT-OWNER)
(define-data-var next-escrow-id uint u0)

;; Define escrow map with all fields
(define-map escrows
  uint
  {
    seller: principal,
    buyer: principal,
    amount: uint,
    status: (string-ascii 20),
    creation-time: uint,
    expiration-time: uint,
    dispute-reason: (optional (string-utf8 500)),
    rating: (optional uint)
  }
)

;; Define user stats map
(define-map user-stats
  principal
  {
    total-transactions: uint,
    successful-transactions: uint,
    disputed-transactions: uint,
    total-volume: uint,
    average-rating: uint
  }
)

;; Admin functions
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin-address)) ERR-NOT-AUTHORIZED)
    (var-set admin-address new-admin)
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-admin)
  (var-get admin-address)
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows escrow-id)
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats user)
)

(define-read-only (get-next-escrow-id)
  (var-get next-escrow-id)
)

;; Private helper functions
(define-private (update-user-stats (user principal) (amount uint))
  (let
    (
      (existing-stats (default-to
        {
          total-transactions: u0,
          successful-transactions: u0,
          disputed-transactions: u0,
          total-volume: u0,
          average-rating: u0
        }
        (map-get? user-stats user)
      ))
    )
    (map-set user-stats user
      (merge existing-stats {
        total-transactions: (+ (get total-transactions existing-stats) u1),
        total-volume: (+ (get total-volume existing-stats) amount)
      })
    )
  )
)

(define-private (update-success-stats (user principal))
  (let
    (
      (existing-stats (default-to
        {
          total-transactions: u0,
          successful-transactions: u0,
          disputed-transactions: u0,
          total-volume: u0,
          average-rating: u0
        }
        (map-get? user-stats user)
      ))
    )
    (map-set user-stats user
      (merge existing-stats {
        successful-transactions: (+ (get successful-transactions existing-stats) u1)
      })
    )
  )
)

(define-private (update-dispute-stats (user principal))
  (let
    (
      (existing-stats (default-to
        {
          total-transactions: u0,
          successful-transactions: u0,
          disputed-transactions: u0,
          total-volume: u0,
          average-rating: u0
        }
        (map-get? user-stats user)
      ))
    )
    (map-set user-stats user
      (merge existing-stats {
        disputed-transactions: (+ (get disputed-transactions existing-stats) u1)
      })
    )
  )
)

;; Create escrow with timeout
(define-public (create-escrow (buyer principal) (amount uint) (timeout uint))
  (let
    (
      (escrow-id (var-get next-escrow-id))
      (creation-block block-height)
      (expiration-block (+ block-height timeout))
    )
    (asserts! (> amount u0) ERR-INSUFFICIENT-AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set escrows escrow-id
      {
        seller: tx-sender,
        buyer: buyer,
        amount: amount,
        status: "pending",
        creation-time: creation-block,
        expiration-time: expiration-block,
        dispute-reason: none,
        rating: none
      }
    )
    
    ;; Update user stats
    (update-user-stats tx-sender amount)
    (update-user-stats buyer u0)
    
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)
;; Release escrow without rating (backward compatibility)
(define-public (release-escrow-simple (escrow-id uint))
  (release-escrow escrow-id u0)
)

;; Release escrow with rating
(define-public (release-escrow (escrow-id uint) (rating uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) ERR-NOT-FOUND))
      (fee (/ (* (get amount escrow) ESCROW-FEE) u10000))
    )
    (asserts! (is-eq (get status escrow) "pending") ERR-INVALID-STATUS)
    (asserts! (is-eq tx-sender (get buyer escrow)) ERR-NOT-AUTHORIZED)
    (asserts! (<= rating MAX-RATING) ERR-INVALID-RATING)
    (asserts! (< block-height (get expiration-time escrow)) ERR-EXPIRED)
    
    (try! (as-contract (stx-transfer? (- (get amount escrow) fee) tx-sender (get seller escrow))))
    (try! (as-contract (stx-transfer? fee tx-sender CONTRACT-OWNER)))
    
    (map-set escrows escrow-id
      (merge escrow {
        status: "completed",
        rating: (if (> rating u0) (some rating) none)
      })
    )
    
    ;; Update success stats
    (update-success-stats (get seller escrow))
    (update-success-stats (get buyer escrow))
    
    (ok true)
  )
)

;; Initiate dispute
(define-public (dispute-escrow (escrow-id uint) (reason (string-utf8 500)))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get status escrow) "pending") ERR-INVALID-STATUS)
   (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) ERR-NOT-AUTHORIZED)
    
    (map-set escrows escrow-id
      (merge escrow {
        status: "disputed",
        dispute-reason: (some reason)
      })
    )
  ;; Update dispute stats
    (update-dispute-stats (get seller escrow))
    (update-dispute-stats (get buyer escrow))
    
    (ok true)
  )
)
;; Resolve dispute (admin only)
(define-public (resolve-dispute (escrow-id uint) (refund-percentage uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) ERR-NOT-FOUND))
      (fee (/ (* (get amount escrow) ESCROW-FEE) u10000))
      (refund-amount (/ (* (get amount escrow) refund-percentage) u100))
      (seller-amount (- (- (get amount escrow) refund-amount) fee))
    )
    (asserts! (is-eq tx-sender (var-get admin-address)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status escrow) "disputed") ERR-INVALID-STATUS)
    (asserts! (<= refund-percentage u100) ERR-INVALID-PERCENTAGE)
    
    (if (> refund-amount u0)
      (try! (as-contract (stx-transfer? refund-amount tx-sender (get buyer escrow))))
      true
    )
    
    (if (> seller-amount u0)
      (try! (as-contract (stx-transfer? seller-amount tx-sender (get seller escrow))))
      true
    )
    
    (try! (as-contract (stx-transfer? fee tx-sender CONTRACT-OWNER)))
    
    (map-set escrows escrow-id
      (merge escrow { status: "resolved" })
    )
    
    (ok true)
  )
)

;; Cancel expired escrow (refund to seller)
(define-public (cancel-expired-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get status escrow) "pending") ERR-INVALID-STATUS)
    (asserts! (>= block-height (get expiration-time escrow)) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq tx-sender (get seller escrow)) (is-eq tx-sender (var-get admin-address))) ERR-NOT-AUTHORIZED)
    
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get seller escrow))))
    
    (map-set escrows escrow-id
      (merge escrow { status: "cancelled" })
    )
    
    (ok true)
  )
)
