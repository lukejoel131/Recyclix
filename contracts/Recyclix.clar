(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_INVALID_MATERIAL (err u103))
(define-constant ERR_ALREADY_VERIFIED (err u104))
(define-constant ERR_NOT_FOUND (err u105))
(define-constant ERR_INVALID_VERIFIER (err u106))
(define-constant ERR_LEADERBOARD_NOT_FOUND (err u107))
(define-constant ERR_ACHIEVEMENT_EXISTS (err u108))
(define-constant ERR_INVALID_SEASON (err u109))
(define-constant ERR_INSUFFICIENT_TOKENS (err u110))
(define-constant ERR_INVALID_CREDIT_AMOUNT (err u111))
(define-constant ERR_TRADE_NOT_FOUND (err u112))
(define-constant ERR_TRADE_EXPIRED (err u113))
(define-constant ERR_CANNOT_BUY_OWN_TRADE (err u114))
(define-constant ERR_CERTIFICATE_NOT_FOUND (err u115))

(define-fungible-token recyclix-token)
(define-fungible-token carbon-credit)

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
(define-data-var current-season uint u1)
(define-data-var season-start-block uint u0)
(define-data-var season-duration uint u1008)
(define-data-var token-to-credit-rate uint u10)
(define-data-var next-trade-id uint u1)
(define-data-var next-certificate-id uint u1)

(define-map leaderboard-scores
  { season: uint, user: principal }
  {
    total-score: uint,
    recycling-streak: uint,
    material-diversity-count: uint,
    last-submission-block: uint,
    rank-position: uint
  }
)

(define-map season-leaderboard
  { season: uint, position: uint }
  {
    user: principal,
    score: uint
  }
)

(define-map user-achievements
  { user: principal, achievement-id: uint }
  {
    unlocked-at-block: uint,
    season-earned: uint
  }
)

(define-map achievement-definitions
  { achievement-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 100),
    score-threshold: uint,
    badge-type: (string-ascii 20)
  }
)

(define-map material-streaks
  { user: principal }
  {
    current-streak: uint,
    max-streak: uint,
    last-submission-block: uint,
    materials-used: (list 10 (string-ascii 20))
  }
)

;; Carbon credit marketplace data structures
(define-map carbon-trades
  { trade-id: uint }
  {
    seller: principal,
    credits-amount: uint,
    price-per-credit: uint,
    expires-at-block: uint,
    active: bool
  }
)

(define-map carbon-certificates
  { certificate-id: uint }
  {
    owner: principal,
    credits-offset: uint,
    generated-at-block: uint,
    purpose: (string-ascii 100),
    verified: bool
  }
)

(define-map carbon-conversions
  { user: principal }
  {
    total-tokens-converted: uint,
    total-credits-generated: uint,
    last-conversion-block: uint
  }
)

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

(define-private (calculate-user-score (user principal))
  (let ((stats (default-to 
          { total-submissions: u0, total-weight: u0, total-tokens-earned: u0, verified-submissions: u0 }
          (map-get? user-stats { user: user })))
        (streak-data (default-to 
          { current-streak: u0, max-streak: u0, last-submission-block: u0, materials-used: (list) }
          (map-get? material-streaks { user: user }))))
    (+ 
      (* (get verified-submissions stats) u100)
      (* (get total-weight stats) u10)
      (* (get max-streak streak-data) u200)
      (* (len (get materials-used streak-data)) u150)
    )
  )
)

(define-private (update-material-streak (user principal) (material-type (string-ascii 20)))
  (let ((current-streak (default-to 
          { current-streak: u0, max-streak: u0, last-submission-block: u0, materials-used: (list) }
          (map-get? material-streaks { user: user })))
        (current-block stacks-block-height)
        (last-block (get last-submission-block current-streak)))
    (let ((is-consecutive (or (is-eq last-block u0) (<= (- current-block last-block) u144)))
          (new-streak (if is-consecutive (+ (get current-streak current-streak) u1) u1))
          (new-max (if (> new-streak (get max-streak current-streak)) new-streak (get max-streak current-streak)))
          (updated-materials (if (is-none (index-of (get materials-used current-streak) material-type))
                                (unwrap-panic (as-max-len? (append (get materials-used current-streak) material-type) u10))
                                (get materials-used current-streak))))
      (map-set material-streaks
        { user: user }
        {
          current-streak: new-streak,
          max-streak: new-max,
          last-submission-block: current-block,
          materials-used: updated-materials
        }
      )
    )
  )
)

(define-private (update-leaderboard-score (user principal))
  (let ((active-season (var-get current-season))
        (user-score (calculate-user-score user))
        (existing-score (default-to 
          { total-score: u0, recycling-streak: u0, material-diversity-count: u0, last-submission-block: u0, rank-position: u0 }
          (map-get? leaderboard-scores { season: active-season, user: user })))
        (streak-data (default-to 
          { current-streak: u0, max-streak: u0, last-submission-block: u0, materials-used: (list) }
          (map-get? material-streaks { user: user }))))
    (map-set leaderboard-scores
      { season: active-season, user: user }
      {
        total-score: user-score,
        recycling-streak: (get current-streak streak-data),
        material-diversity-count: (len (get materials-used streak-data)),
        last-submission-block: stacks-block-height,
        rank-position: (get rank-position existing-score)
      }
    )
  )
)

(define-private (check-achievements (user principal))
  (let ((user-score (calculate-user-score user)))
    (begin
      (if (>= user-score u1000)
        (unwrap-panic (unlock-achievement user u1))
        true
      )
      (if (>= user-score u5000)
        (unwrap-panic (unlock-achievement user u2))
        true
      )
      (if (>= user-score u10000)
        (unwrap-panic (unlock-achievement user u3))
        true
      )
      (ok true)
    )
  )
)

(define-private (unlock-achievement (user principal) (achievement-id uint))
  (let ((existing (map-get? user-achievements { user: user, achievement-id: achievement-id })))
    (if (is-none existing)
      (begin
        (map-set user-achievements
          { user: user, achievement-id: achievement-id }
          {
            unlocked-at-block: stacks-block-height,
            season-earned: (var-get current-season)
          }
        )
        (ok true)
      )
      (ok true)
    )
  )
)

;; Carbon credit conversion and trading functions
(define-private (update-conversion-stats (user principal) (tokens-amount uint) (credits-amount uint))
  (let ((existing (default-to 
          { total-tokens-converted: u0, total-credits-generated: u0, last-conversion-block: u0 }
          (map-get? carbon-conversions { user: user }))))
    (map-set carbon-conversions
      { user: user }
      {
        total-tokens-converted: (+ (get total-tokens-converted existing) tokens-amount),
        total-credits-generated: (+ (get total-credits-generated existing) credits-amount),
        last-conversion-block: stacks-block-height
      }
    )
  )
)

(define-public (convert-tokens-to-credits (token-amount uint))
  (let ((conversion-rate (var-get token-to-credit-rate))
        (credits-to-mint (/ token-amount conversion-rate)))
    (asserts! (> token-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (ft-get-balance recyclix-token tx-sender) token-amount) ERR_INSUFFICIENT_TOKENS)
    (asserts! (> credits-to-mint u0) ERR_INVALID_CREDIT_AMOUNT)
    
    ;; Burn recyclix tokens and mint carbon credits
    (try! (ft-burn? recyclix-token token-amount tx-sender))
    (try! (ft-mint? carbon-credit credits-to-mint tx-sender))
    
    ;; Update conversion statistics
    (update-conversion-stats tx-sender token-amount credits-to-mint)
    
    (ok credits-to-mint)
  )
)

(define-public (create-carbon-trade (credits-amount uint) (price-per-credit uint) (duration-blocks uint))
  (let ((trade-id (var-get next-trade-id))
        (expires-at (+ stacks-block-height duration-blocks)))
    (asserts! (> credits-amount u0) ERR_INVALID_CREDIT_AMOUNT)
    (asserts! (> price-per-credit u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (ft-get-balance carbon-credit tx-sender) credits-amount) ERR_INSUFFICIENT_TOKENS)
    
    ;; Escrow the carbon credits
    (try! (ft-transfer? carbon-credit credits-amount tx-sender (as-contract tx-sender)))
    
    ;; Create trade listing
    (map-set carbon-trades
      { trade-id: trade-id }
      {
        seller: tx-sender,
        credits-amount: credits-amount,
        price-per-credit: price-per-credit,
        expires-at-block: expires-at,
        active: true
      }
    )
    
    (var-set next-trade-id (+ trade-id u1))
    (ok trade-id)
  )
)

(define-public (buy-carbon-credits (trade-id uint))
  (let ((trade (unwrap! (map-get? carbon-trades { trade-id: trade-id }) ERR_TRADE_NOT_FOUND)))
    (asserts! (get active trade) ERR_TRADE_NOT_FOUND)
    (asserts! (<= stacks-block-height (get expires-at-block trade)) ERR_TRADE_EXPIRED)
    (asserts! (not (is-eq tx-sender (get seller trade))) ERR_CANNOT_BUY_OWN_TRADE)
    
    (let ((total-cost (* (get credits-amount trade) (get price-per-credit trade)))
          (seller (get seller trade))
          (credits-amount (get credits-amount trade))
          (buyer tx-sender))
      
      (asserts! (>= (ft-get-balance recyclix-token tx-sender) total-cost) ERR_INSUFFICIENT_TOKENS)
      
      ;; Transfer payment to seller
      (try! (ft-transfer? recyclix-token total-cost tx-sender seller))
      
      ;; Transfer carbon credits to buyer
      (try! (as-contract (ft-transfer? carbon-credit credits-amount tx-sender buyer)))
      
      ;; Mark trade as inactive
      (map-set carbon-trades
        { trade-id: trade-id }
        (merge trade { active: false })
      )
      
      (ok credits-amount)
    )
  )
)

(define-public (cancel-carbon-trade (trade-id uint))
  (let ((trade (unwrap! (map-get? carbon-trades { trade-id: trade-id }) ERR_TRADE_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get seller trade)) ERR_UNAUTHORIZED)
    (asserts! (get active trade) ERR_TRADE_NOT_FOUND)
    
    ;; Return escrowed credits to seller
    (try! (as-contract (ft-transfer? carbon-credit (get credits-amount trade) tx-sender (get seller trade))))
    
    ;; Mark trade as inactive
    (map-set carbon-trades
      { trade-id: trade-id }
      (merge trade { active: false })
    )
    
    (ok true)
  )
)

(define-public (generate-carbon-certificate (credits-to-offset uint) (purpose (string-ascii 100)))
  (let ((certificate-id (var-get next-certificate-id)))
    (asserts! (> credits-to-offset u0) ERR_INVALID_CREDIT_AMOUNT)
    (asserts! (>= (ft-get-balance carbon-credit tx-sender) credits-to-offset) ERR_INSUFFICIENT_TOKENS)
    
    ;; Burn carbon credits for permanent offset
    (try! (ft-burn? carbon-credit credits-to-offset tx-sender))
    
    ;; Generate certificate
    (map-set carbon-certificates
      { certificate-id: certificate-id }
      {
        owner: tx-sender,
        credits-offset: credits-to-offset,
        generated-at-block: stacks-block-height,
        purpose: purpose,
        verified: true
      }
    )
    
    (var-set next-certificate-id (+ certificate-id u1))
    (ok certificate-id)
  )
)

(define-public (set-conversion-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-rate u0) ERR_INVALID_AMOUNT)
    (var-set token-to-credit-rate new-rate)
    (ok true)
  )
)

(define-public (initialize-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (try! (ft-mint? recyclix-token u1000000000000 CONTRACT_OWNER))
    (var-set total-supply u1000000000000)
    (var-set season-start-block stacks-block-height)
    (map-set material-rates { material-type: "plastic" } { rate-per-kg: u100 })
    (map-set material-rates { material-type: "glass" } { rate-per-kg: u150 })
    (map-set material-rates { material-type: "metal" } { rate-per-kg: u200 })
    (map-set material-rates { material-type: "paper" } { rate-per-kg: u75 })
    (map-set material-rates { material-type: "electronics" } { rate-per-kg: u500 })
    (map-set achievement-definitions { achievement-id: u1 } 
      { name: "Recycling Novice", description: "Complete your first 1000 points worth of recycling activities", score-threshold: u1000, badge-type: "bronze" })
    (map-set achievement-definitions { achievement-id: u2 } 
      { name: "Eco Warrior", description: "Achieve 5000 points through consistent recycling efforts", score-threshold: u5000, badge-type: "silver" })
    (map-set achievement-definitions { achievement-id: u3 } 
      { name: "Green Champion", description: "Reach 10000 points demonstrating recycling mastery", score-threshold: u10000, badge-type: "gold" })
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
      (update-material-streak user (get material-type submission))
      (update-leaderboard-score user)
      (unwrap-panic (check-achievements user))
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

(define-public (start-new-season)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (let ((current-block stacks-block-height)
          (season-start (var-get season-start-block))
          (duration (var-get season-duration)))
      (asserts! (>= (- current-block season-start) duration) ERR_INVALID_SEASON)
      (var-set current-season (+ (var-get current-season) u1))
      (var-set season-start-block current-block)
      (ok (var-get current-season))
    )
  )
)

(define-public (update-leaderboard-rankings)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok true)
  )
)

(define-read-only (get-leaderboard-position (season uint) (position uint))
  (map-get? season-leaderboard { season: season, position: position })
)

(define-read-only (get-user-leaderboard-score (season uint) (user principal))
  (map-get? leaderboard-scores { season: season, user: user })
)

(define-read-only (get-user-achievements (user principal))
  (let ((achievement-1 (map-get? user-achievements { user: user, achievement-id: u1 }))
        (achievement-2 (map-get? user-achievements { user: user, achievement-id: u2 }))
        (achievement-3 (map-get? user-achievements { user: user, achievement-id: u3 })))
    {
      bronze-badge: achievement-1,
      silver-badge: achievement-2,
      gold-badge: achievement-3
    }
  )
)

(define-read-only (get-achievement-definition (achievement-id uint))
  (map-get? achievement-definitions { achievement-id: achievement-id })
)

(define-read-only (get-user-streak-data (user principal))
  (map-get? material-streaks { user: user })
)

(define-read-only (get-current-season-info)
  {
    current-season: (var-get current-season),
    season-start-block: (var-get season-start-block),
    season-duration: (var-get season-duration),
    blocks-remaining: (- (+ (var-get season-start-block) (var-get season-duration)) stacks-block-height)
  }
)

(define-read-only (get-user-performance-stats (user principal))
  (let ((stats (default-to 
          { total-submissions: u0, total-weight: u0, total-tokens-earned: u0, verified-submissions: u0 }
          (map-get? user-stats { user: user })))
        (streak-data (default-to 
          { current-streak: u0, max-streak: u0, last-submission-block: u0, materials-used: (list) }
          (map-get? material-streaks { user: user })))
        (score (calculate-user-score user)))
    (merge stats {
      current-score: score,
      recycling-streak: (get current-streak streak-data),
      max-streak: (get max-streak streak-data),
      materials-recycled: (len (get materials-used streak-data))
    })
  )
)

;; Carbon credit system read-only functions
(define-read-only (get-carbon-credit-balance (user principal))
  (ft-get-balance carbon-credit user)
)

(define-read-only (get-conversion-rate)
  (var-get token-to-credit-rate)
)

(define-read-only (get-carbon-trade (trade-id uint))
  (map-get? carbon-trades { trade-id: trade-id })
)

(define-read-only (get-carbon-certificate (certificate-id uint))
  (map-get? carbon-certificates { certificate-id: certificate-id })
)

(define-read-only (get-user-conversion-stats (user principal))
  (map-get? carbon-conversions { user: user })
)

(define-read-only (get-active-trades-count)
  (var-get next-trade-id)
)

(define-read-only (get-total-certificates-issued)
  (var-get next-certificate-id)
)

(define-read-only (calculate-carbon-impact (token-amount uint))
  (let ((conversion-rate (var-get token-to-credit-rate)))
    (/ token-amount conversion-rate)
  )
)


