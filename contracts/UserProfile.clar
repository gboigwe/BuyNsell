;; UserProfile: Manages user profiles, ratings, and reputation for BuyNsell marketplace

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_USER_NOT_FOUND (err u101))
(define-constant ERR_INVALID_RATING (err u102))
(define-constant ERR_SELF_RATING (err u103))
(define-constant ERR_ALREADY_REGISTERED (err u104))
(define-constant ERR_INVALID_INPUT (err u105))
(define-constant ERR_DATA_STORE_FAILURE (err u106))

;; Data Maps
(define-map Users principal
  {
    username: (string-utf8 64),
    bio: (string-utf8 256),
    email: (string-utf8 64),
    registration-date: uint,
    total-ratings: uint,
    rating-sum: uint,
    reputation-score: uint
  }
)

(define-map UserAuthorization principal bool)

;; Private Functions

(define-private (is-valid-string (str (string-utf8 256)) (max-len uint))
  (and (>= (len str) u1) (<= (len str) max-len))
)

(define-private (set-user-data (user principal) (data {
    username: (string-utf8 64),
    bio: (string-utf8 256),
    email: (string-utf8 64),
    registration-date: uint,
    total-ratings: uint,
    rating-sum: uint,
    reputation-score: uint
  }))
  (ok (map-set Users user data))
)

;; Read-only functions

(define-read-only (get-user-profile (user principal))
  (map-get? Users user)
)

(define-read-only (get-user-rating (user principal))
  (match (map-get? Users user)
    user-data (let (
      (total-ratings (get total-ratings user-data))
      (rating-sum (get rating-sum user-data))
    )
    (if (> total-ratings u0)
      (ok (/ rating-sum total-ratings))
      (ok u0)))
    (err ERR_USER_NOT_FOUND)
  )
)

(define-read-only (is-user-authorized (user principal))
  (default-to false (map-get? UserAuthorization user))
)

;; Public functions

(define-public (register-user (username (string-utf8 64)) (bio (string-utf8 256)) (email (string-utf8 64)))
  (let (
    (existing-user (map-get? Users tx-sender))
  )
  (asserts! (is-none existing-user) (err ERR_ALREADY_REGISTERED))
  (asserts! (and 
              (is-valid-string username u64)
              (is-valid-string bio u256)
              (is-valid-string email u64)) 
            (err ERR_INVALID_INPUT))
  (unwrap! (set-user-data tx-sender
    {
      username: username,
      bio: bio,
      email: email,
      registration-date: block-height,
      total-ratings: u0,
      rating-sum: u0,
      reputation-score: u0
    })
    (err ERR_DATA_STORE_FAILURE))
  (ok true)))

(define-public (update-profile (bio (string-utf8 256)) (email (string-utf8 64)))
  (match (map-get? Users tx-sender)
    user-data 
      (begin
        (asserts! (and 
                    (is-valid-string bio u256)
                    (is-valid-string email u64)) 
                  (err ERR_INVALID_INPUT))
        (unwrap! (set-user-data tx-sender
          (merge user-data
            {
              bio: bio,
              email: email
            }
          ))
          (err ERR_DATA_STORE_FAILURE))
        (ok true))
    (err ERR_USER_NOT_FOUND)
  )
)

(define-public (rate-user (user principal) (rating uint))
  (match (map-get? Users user)
    user-data
      (begin
        (asserts! (not (is-eq tx-sender user)) (err ERR_SELF_RATING))
        (asserts! (and (>= rating u1) (<= rating u5)) (err ERR_INVALID_RATING))
        (unwrap! (set-user-data user
          (merge user-data
            {
              total-ratings: (+ (get total-ratings user-data) u1),
              rating-sum: (+ (get rating-sum user-data) rating)
            }
          ))
          (err ERR_DATA_STORE_FAILURE))
        (ok true))
    (err ERR_USER_NOT_FOUND)
  )
)

(define-public (calculate-reputation (user principal))
  (match (map-get? Users user)
    user-data
      (let (
        (total-ratings (get total-ratings user-data))
        (rating-sum (get rating-sum user-data))
        (avg-rating (if (> total-ratings u0) (/ rating-sum total-ratings) u0))
        (new-reputation (+ (* avg-rating u20) (* total-ratings u2)))
      )
      (unwrap! (set-user-data user
        (merge user-data
          {
            reputation-score: new-reputation
          }
        ))
        (err ERR_DATA_STORE_FAILURE))
      (ok new-reputation))
    (err ERR_USER_NOT_FOUND)
  )
)

(define-public (authorize-user (user principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_NOT_AUTHORIZED))
    (ok (map-set UserAuthorization user true))
  )
)

(define-public (revoke-authorization (user principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_NOT_AUTHORIZED))
    (ok (map-set UserAuthorization user false))
  )
)

;; Initialize contract
(begin
  (map-set UserAuthorization CONTRACT_OWNER true)
  (print "UserProfile contract initialized")
  (ok true)
)
