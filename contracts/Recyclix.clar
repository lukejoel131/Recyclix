(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_INVALID_MATERIAL (err u103))
(define-constant ERR_ALREADY_VERIFIED (err u104))
(define-constant ERR_NOT_FOUND (err u105))
(define-constant ERR_INVALID_VERIFIER (err u106))

(define-fungible-token recyclix-token)

(define-data-var token-name (string-ascii 32) "Recyclix Token")
(define-data-var token-symbol (string-ascii 10) "RCX")
(define-data-var token-decimals uint u6)
(define-data-var total-supply uint u0)

(define-map material-rates
  { material-type: (string-ascii 20) }
  { rate-per-kg: uint }
)

(define-map recycling-submissions
  { submission-id: uint }
  {
    user: principal,
    material-type: (string-ascii 20),
    weight-kg: uint,
    timestamp: uint,
    verified: bool,
    verifier: (optional principal),
    tokens-earned: uint
  }
)

(define-map user-stats
  { user: principal }
  {
    total-submissions: uint,
    total-weight: uint,
    total-tokens-earned: uint,
    verified-submissions: uint
  }
)

(define-map authorized-verifiers
  { verifier: principal }
  { active: bool }
)

(define-data-var next-submission-id uint u1)

(define-private (is-authorized-verifier (verifier principal))
  (default-to false (get active (map-get? authorized-verifiers { verifier: verifier })))
)

(define-private (get-material-rate (material-type (string-ascii 20)))
  (default-to u0 (get rate-per-kg (map-get? material-rates { material-type: material-type })))
)

(define-private (calculate-tokens (material-type (string-ascii 20)) (weight-kg uint))
  (let ((rate (get-material-rate material-type)))
    (* rate weight-kg)
  )
)

(define-private (update-user-stats (user principal) (weight uint) (tokens uint) (verified bool))
  (let ((current-stats (default-to 
    { total-submissions: u0, total-weight: u0, total-tokens-earned: u0, verified-submissions: u0 }
    (map-get? user-stats { user: user }))))
    (map-set user-stats
      { user: user }
      {
        total-submissions: (+ (get total-submissions current-stats) u1),
        total-weight: (+ (get total-weight current-stats) weight),
        total-tokens-earned: (+ (get total-tokens-earned current-stats) tokens),
        verified-submissions: (+ (get verified-submissions current-stats) (if verified u1 u0))
      }
    )
  )
)

(define-public (initialize-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (try! (ft-mint? recyclix-token u1000000000000 CONTRACT_OWNER))
    (var-set total-supply u1000000000000)
    (map-set material-rates { material-type: "plastic" } { rate-per-kg: u100 })
    (map-set material-rates { material-type: "glass" } { rate-per-kg: u150 })
    (map-set material-rates { material-type: "metal" } { rate-per-kg: u200 })
    (map-set material-rates { material-type: "paper" } { rate-per-kg: u75 })
    (map-set material-rates { material-type: "electronics" } { rate-per-kg: u500 })
    (ok true)
  )
)

(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-verifiers { verifier: verifier } { active: true })
    (ok true)
  )
)

(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-verifiers { verifier: verifier } { active: false })
    (ok true)
  )
)

(define-public (set-material-rate (material-type (string-ascii 20)) (rate-per-kg uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> rate-per-kg u0) ERR_INVALID_AMOUNT)
    (map-set material-rates { material-type: material-type } { rate-per-kg: rate-per-kg })
    (ok true)
  )
)

(define-public (submit-recycling (material-type (string-ascii 20)) (weight-kg uint))
  (let ((submission-id (var-get next-submission-id))
        (tokens-to-earn (calculate-tokens material-type weight-kg)))
    (asserts! (> weight-kg u0) ERR_INVALID_AMOUNT)
    (asserts! (> (get-material-rate material-type) u0) ERR_INVALID_MATERIAL)
    
    (map-set recycling-submissions
      { submission-id: submission-id }
      {
        user: tx-sender,
        material-type: material-type,
        weight-kg: weight-kg,
        timestamp: stacks-block-height,
        verified: false,
        verifier: none,
        tokens-earned: u0
      }
    )
    
    (update-user-stats tx-sender weight-kg u0 false)
    (var-set next-submission-id (+ submission-id u1))
    (ok submission-id)
  )
)

(define-public (verify-submission (submission-id uint))
  (let ((submission (unwrap! (map-get? recycling-submissions { submission-id: submission-id }) ERR_NOT_FOUND)))
    (asserts! (is-authorized-verifier tx-sender) ERR_INVALID_VERIFIER)
    (asserts! (not (get verified submission)) ERR_ALREADY_VERIFIED)
    
    (let ((tokens-to-mint (calculate-tokens (get material-type submission) (get weight-kg submission)))
          (user (get user submission)))
      
      (try! (ft-mint? recyclix-token tokens-to-mint user))
      
      (map-set recycling-submissions
        { submission-id: submission-id }
        (merge submission {
          verified: true,
          verifier: (some tx-sender),
          tokens-earned: tokens-to-mint
        })
      )
      
      (update-user-stats user u0 tokens-to-mint true)
      (ok tokens-to-mint)
    )
  )
)

(define-public (transfer-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (ft-transfer? recyclix-token amount tx-sender recipient)
  )
)

(define-read-only (get-token-balance (user principal))
  (ft-get-balance recyclix-token user)
)

(define-read-only (get-submission (submission-id uint))
  (map-get? recycling-submissions { submission-id: submission-id })
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

(define-read-only (get-material-rate-info (material-type (string-ascii 20)))
  (map-get? material-rates { material-type: material-type })
)

(define-read-only (is-verifier (verifier principal))
  (is-authorized-verifier verifier)
)

(define-read-only (get-token-info)
  {
    name: (var-get token-name),
    symbol: (var-get token-symbol),
    decimals: (var-get token-decimals),
    total-supply: (var-get total-supply)
  }
)

(define-read-only (get-next-submission-id)
  (var-get next-submission-id)
)