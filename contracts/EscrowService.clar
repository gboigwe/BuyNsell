;; EscrowService: Manages secure transactions between buyers and sellers

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ESCROW_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_RELEASED (err u102))
(define-constant ERR_TRANSFER_FAILED (err u103))
(define-constant ERR_INVALID_ESCROW_ID (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_INVALID_SELLER (err u106))

;; Define data maps
(define-map Escrows
  { escrow-id: uint }
  {
    buyer: principal,
    seller: principal,
    amount: uint,
    state: (string-ascii 10)
  }
)

;; Define variables
(define-data-var last-escrow-id uint u0)

;; Helper functions
(define-private (is-valid-seller (seller principal))
  (and 
    (not (is-eq seller tx-sender))
    (not (is-eq seller (as-contract tx-sender)))
  )
)

(define-private (is-valid-escrow-id (escrow-id uint))
  (<= escrow-id (var-get last-escrow-id))
)

;; Create a new escrow
(define-public (create-escrow (seller principal) (amount uint))
  (let
    (
      (escrow-id (+ (var-get last-escrow-id) u1))
    )
    (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))
    (asserts! (is-valid-seller seller) (err ERR_INVALID_SELLER))
    (match (stx-transfer? amount tx-sender (as-contract tx-sender))
      success
        (begin
          (map-set Escrows
            { escrow-id: escrow-id }
            {
              buyer: tx-sender,
              seller: seller,
              amount: amount,
              state: "locked"
            }
          )
          (var-set last-escrow-id escrow-id)
          (ok escrow-id)
        )
      error (err ERR_TRANSFER_FAILED)
    )
  )
)

;; Release funds to the seller
(define-public (release-funds (escrow-id uint))
  (begin
    (asserts! (is-valid-escrow-id escrow-id) (err ERR_INVALID_ESCROW_ID))
    (let
      (
        (escrow (unwrap! (map-get? Escrows { escrow-id: escrow-id }) (err ERR_ESCROW_NOT_FOUND)))
        (seller (get seller escrow))
        (amount (get amount escrow))
      )
      (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_NOT_AUTHORIZED))
      (asserts! (is-eq (get state escrow) "locked") (err ERR_ALREADY_RELEASED))
      (match (as-contract (stx-transfer? amount tx-sender seller))
        success 
          (begin
            (map-set Escrows
              { escrow-id: escrow-id }
              (merge escrow { state: "released" })
            )
            (ok true)
          )
        error (err ERR_TRANSFER_FAILED)
      )
    )
  )
)

;; Refund buyer
(define-public (refund-buyer (escrow-id uint))
  (begin
    (asserts! (is-valid-escrow-id escrow-id) (err ERR_INVALID_ESCROW_ID))
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
            (ok true)
          )
        error (err ERR_TRANSFER_FAILED)
      )
    )
  )
)

;; Get escrow details
(define-read-only (get-escrow (escrow-id uint))
  (begin
    (asserts! (is-valid-escrow-id escrow-id) (err ERR_INVALID_ESCROW_ID))
    (match (map-get? Escrows { escrow-id: escrow-id })
      escrow (ok escrow)
      (err ERR_ESCROW_NOT_FOUND)
    )
  )
)

;; Get last escrow ID
(define-read-only (get-last-escrow-id)
  (ok (var-get last-escrow-id))
)
