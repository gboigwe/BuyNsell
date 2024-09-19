;; BuyNsell Token (BST) Contract

;; Define token properties
(define-fungible-token bst u1000000000000)

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_OWNER_ONLY (err u100))
(define-constant ERR_NOT_AUTHORIZED (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_INVALID_RECIPIENT (err u104))
(define-constant ERR_INVALID_URI (err u105))

;; Define variables
(define-data-var token-name (string-ascii 32) "BuyNsell Token")
(define-data-var token-symbol (string-ascii 10) "BST")
(define-data-var token-decimals uint u6)
(define-data-var token-uri (optional (string-utf8 256)) none)

;; Helper function to check if a principal is a valid recipient
(define-private (is-valid-recipient (recipient principal))
  (not (is-eq recipient (as-contract tx-sender))))

;; SIP-010: Transfer
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (begin
        (asserts! (is-eq tx-sender sender) ERR_NOT_AUTHORIZED)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (<= amount (ft-get-balance bst sender)) ERR_INSUFFICIENT_BALANCE)
        (asserts! (is-valid-recipient recipient) ERR_INVALID_RECIPIENT)
        (try! (ft-transfer? bst amount sender recipient))
        (match memo to-print (print to-print) 0x)
        (ok true)
    )
)

;; SIP-010: Get name
(define-read-only (get-name)
    (ok (var-get token-name))
)

;; SIP-010: Get symbol
(define-read-only (get-symbol)
    (ok (var-get token-symbol))
)

;; SIP-010: Get decimals
(define-read-only (get-decimals)
    (ok (var-get token-decimals))
)

;; SIP-010: Get balance
(define-read-only (get-balance (who principal))
    (ok (ft-get-balance bst who))
)

;; SIP-010: Get total supply
(define-read-only (get-total-supply)
    (ok (ft-get-supply bst))
)

;; SIP-010: Get token URI
(define-read-only (get-token-uri)
    (ok (var-get token-uri))
)

;; Mint new tokens (only contract owner)
(define-public (mint (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (is-valid-recipient recipient) ERR_INVALID_RECIPIENT)
        (ft-mint? bst amount recipient)
    )
)

;; Burn tokens
(define-public (burn (amount uint) (sender principal))
    (begin
        (asserts! (is-eq tx-sender sender) ERR_NOT_AUTHORIZED)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (<= amount (ft-get-balance bst sender)) ERR_INSUFFICIENT_BALANCE)
        (ft-burn? bst amount sender)
    )
)

;; Update token URI (only contract owner)
(define-public (set-token-uri (new-uri (optional (string-utf8 256))))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
        (match new-uri
            uri (begin
                (asserts! (<= (len uri) u256) ERR_INVALID_URI)
                (var-set token-uri (some uri))
            )
            (var-set token-uri none)
        )
        (ok true)
    )
)

;; Initialize the contract
(begin
    ;; Mint initial supply to contract owner
    (try! (ft-mint? bst u1000000000000 CONTRACT_OWNER))
    ;; Print a deploy message
    (print "BuyNsell Token (BST) contract deployed successfully")
)
