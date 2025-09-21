;;P2P-ESCROW-Contract
;;A peer-to-peer escrow smart contract that holds funds securely between two parties until predetermined conditions are met, enabling trustless transactions without intermediaries.

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

