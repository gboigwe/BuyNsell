;; EscrowService: Manages secure transactions between buyers and sellers

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ESCROW_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_ALREADY_RELEASED (err u104))
(define-constant ERR_NOT_BUYER_OR_SELLER (err u105))
(define-constant ERR_TOO_EARLY_TO_RELEASE (err u106))
(define-constant ERR_INVALID_STATE (err u107))
(define-constant ERR_TRANSFER_FAILED (err u108))
(define-constant ESCROW_LOCK_PERIOD u144) ;; 24 hours in blocks (assuming 10-minute block times)

;; Data Maps
(define-map Escrows
  { escrow-id: uint }
  {
    buyer: principal,
    seller: principal,
    amount: uint,
    state: (string-ascii 20),
    created-at: uint,
    release-height: uint
  }
)

(define-data-var last-escrow-id uint u0)

;; Public Functions
(define-public (create-escrow (seller principal) (amount uint))
  (let
    (
      (escrow-id (+ (var-get last-escrow-id) u1))
      (release-height (+ block-height ESCROW_LOCK_PERIOD))
    )
    (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))
    (match (stx-transfer? amount tx-sender (as-contract tx-sender))
      success 
        (begin
          (map-set Escrows
            { escrow-id: escrow-id }
            {
              buyer: tx-sender,
              seller: seller,
              amount: amount,
              state: "locked",
              created-at: block-height,
              release-height: release-height
            }
          )
          (var-set last-escrow-id escrow-id)
          (ok escrow-id))
      error (err ERR_TRANSFER_FAILED)
    )
  )
)

(define-public (release-funds (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? Escrows { escrow-id: escrow-id }) (err ERR_ESCROW_NOT_FOUND)))
      (buyer (get buyer escrow))
      (seller (get seller escrow))
      (amount (get amount escrow))
    )
    (asserts! (or (is-eq tx-sender buyer) (is-eq tx-sender seller)) (err ERR_NOT_BUYER_OR_SELLER))
    (asserts! (is-eq (get state escrow) "locked") (err ERR_ALREADY_RELEASED))
    (asserts! (>= block-height (get release-height escrow)) (err ERR_TOO_EARLY_TO_RELEASE))
    (match (as-contract (stx-transfer? amount tx-sender seller))
      success 
        (begin
          (map-set Escrows
            { escrow-id: escrow-id }
            (merge escrow { state: "released" })
          )
          (ok true))
      error (err ERR_TRANSFER_FAILED)
    )
  )
)

(define-public (refund-buyer (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? Escrows { escrow-id: escrow-id }) (err ERR_ESCROW_NOT_FOUND)))
      (buyer (get buyer escrow))
      (amount (get amount escrow))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_NOT_AUTHORIZED))
    (asserts! (is-eq (get state escrow) "locked") (err ERR_ALREADY_RELEASED))
    (match (as-contract (stx-transfer? amount tx-sender buyer))
      success 
        (begin
          (map-set Escrows
            { escrow-id: escrow-id }
            (merge escrow { state: "refunded" })
          )
          (ok true))
      error (err ERR_TRANSFER_FAILED)
    )
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? Escrows { escrow-id: escrow-id })
)

(define-read-only (get-last-escrow-id)
  (ok (var-get last-escrow-id))
)

;; Initialize contract
(begin
  (print "EscrowService contract initialized")
  (ok true)
)
