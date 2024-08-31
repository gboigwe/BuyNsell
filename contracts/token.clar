;; BuyNsell Token (BST) Contract

;; Define SIP-010 trait
(define-trait sip-010-trait
  (
    ;; Transfer from the caller to a new principal
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))

    ;; The human-readable name of the token
    (get-name () (response (string-ascii 32) uint))

    ;; The ticker symbol, or empty if none
    (get-symbol () (response (string-ascii 32) uint))

    ;; The number of decimals used, e.g. 6 would mean 1_000_000 represents 1 token
    (get-decimals () (response uint uint))

    ;; The balance of the passed principal
    (get-balance (principal) (response uint uint))

    ;; The current total supply (which does not need to be a constant)
    (get-total-supply () (response uint uint))

    ;; Optional URI for off-chain metadata
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; Implement SIP-010 trait
(impl-trait sip-010-trait)

;; Define token properties
(define-fungible-token bst u1000000000000)

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_OWNER_ONLY (err u100))
(define-constant ERR_NOT_AUTHORIZED (err u101))

;; Define variables
(define-data-var token-name (string-ascii 32) "BuyNsell Token")
(define-data-var token-symbol (string-ascii 10) "BST")
(define-data-var token-decimals uint u6)
(define-data-var token-uri (optional (string-utf8 256)) none)

;; SIP-010: Transfer
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (begin
        (asserts! (is-eq tx-sender sender) ERR_NOT_AUTHORIZED)
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
        (ft-mint? bst amount recipient)
    )
)

;; Burn tokens
(define-public (burn (amount uint) (sender principal))
    (begin
        (asserts! (is-eq tx-sender sender) ERR_NOT_AUTHORIZED)
        (ft-burn? bst amount sender)
    )
)

;; Update token URI (only contract owner)
(define-public (set-token-uri (new-uri (optional (string-utf8 256))))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
        (ok (var-set token-uri new-uri))
    )
)

;; Initialize the contract
(begin
    ;; Mint initial supply to contract owner
    (try! (ft-mint? bst u1000000000000 CONTRACT_OWNER))
    ;; Print a deploy message
    (print "BuyNsell Token (BST) contract deployed successfully")
)
