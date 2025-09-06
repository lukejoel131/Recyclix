;; Recycling Location & Impact Tracker
;; Track recycling locations and calculate environmental impact metrics

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u400))
(define-constant err-not-found (err u401))
(define-constant err-invalid-input (err u402))
(define-constant err-location-exists (err u403))
(define-constant err-location-inactive (err u404))

;; Location types
(define-constant location-type-center u1)
(define-constant location-type-kiosk u2)
(define-constant location-type-collection-point u3)

;; Environmental impact constants (CO2 saved per kg in grams)
(define-constant plastic-co2-saved u1800)   ;; 1.8kg CO2 per kg plastic
(define-constant glass-co2-saved u314)      ;; 0.314kg CO2 per kg glass  
(define-constant metal-co2-saved u1433)     ;; 1.433kg CO2 per kg metal
(define-constant paper-co2-saved u2100)     ;; 2.1kg CO2 per kg paper
(define-constant electronics-co2-saved u6000) ;; 6kg CO2 per kg electronics

;; Data variables
(define-data-var next-location-id uint u1)
(define-data-var total-registered-locations uint u0)
(define-data-var global-co2-saved uint u0)

;; Recycling location registry
(define-map recycling-locations
  { location-id: uint }
  {
    name: (string-ascii 100),
    location-type: uint,
    coordinates: (string-ascii 50),
    operator: principal,
    capacity-rating: uint,
    materials-accepted: (list 5 (string-ascii 20)),
    active: bool,
    registered-at: uint,
    total-submissions: uint,
    total-weight-processed: uint
  }
)

;; User location usage tracking
(define-map user-location-usage
  { user: principal, location-id: uint }
  {
    first-visit: uint,
    total-visits: uint,
    total-weight-submitted: uint,
    materials-submitted: (list 10 (string-ascii 20)),
    last-visit: uint,
    co2-impact: uint
  }
)

;; Location-based submissions tracking
(define-map location-submissions
  { location-id: uint, submission-date: uint }
  {
    daily-weight: uint,
    submission-count: uint,
    materials-breakdown: (list 10 { material: (string-ascii 20), weight: uint })
  }
)

;; Environmental impact tracking
(define-map user-environmental-impact
  { user: principal }
  {
    total-co2-saved: uint,
    materials-diverted: uint,
    equivalent-trees-planted: uint,
    impact-score: uint,
    last-calculated: uint
  }
)

;; Location impact metrics
(define-map location-impact-metrics
  { location-id: uint }
  {
    total-co2-impact: uint,
    total-materials-processed: uint,
    efficiency-rating: uint,
    environmental-score: uint,
    last-updated: uint
  }
)

;; Read-only functions
(define-read-only (get-location-info (location-id uint))
  (map-get? recycling-locations { location-id: location-id })
)

(define-read-only (get-user-location-stats (user principal) (location-id uint))
  (map-get? user-location-usage { user: user, location-id: location-id })
)

(define-read-only (get-location-daily-stats (location-id uint) (date uint))
  (map-get? location-submissions { location-id: location-id, submission-date: date })
)

(define-read-only (get-user-impact (user principal))
  (map-get? user-environmental-impact { user: user })
)

(define-read-only (get-location-impact (location-id uint))
  (map-get? location-impact-metrics { location-id: location-id })
)

(define-read-only (get-global-impact-stats)
  {
    total-locations: (var-get total-registered-locations),
    global-co2-saved: (var-get global-co2-saved),
    next-location-id: (var-get next-location-id)
  }
)

;; Public functions
(define-public (register-recycling-location (name (string-ascii 100)) (location-type uint) (coordinates (string-ascii 50)) (capacity-rating uint) (materials-accepted (list 5 (string-ascii 20))))
  (let ((location-id (var-get next-location-id)))
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (and (>= location-type u1) (<= location-type u3)) err-invalid-input)
    (asserts! (and (>= capacity-rating u1) (<= capacity-rating u5)) err-invalid-input)
    
    (map-set recycling-locations { location-id: location-id }
      {
        name: name,
        location-type: location-type,
        coordinates: coordinates,
        operator: tx-sender,
        capacity-rating: capacity-rating,
        materials-accepted: materials-accepted,
        active: true,
        registered-at: stacks-block-height,
        total-submissions: u0,
        total-weight-processed: u0
      })
    
    (var-set next-location-id (+ location-id u1))
    (var-set total-registered-locations (+ (var-get total-registered-locations) u1))
    (ok location-id)
  )
)

(define-public (record-location-submission (location-id uint) (material-type (string-ascii 20)) (weight-kg uint))
  (let 
    ((location (unwrap! (map-get? recycling-locations { location-id: location-id }) err-not-found))
     (today (/ stacks-block-height u144))  ;; Approximate daily blocks
     (co2-saved (calculate-co2-impact material-type weight-kg))
     (current-usage (default-to 
       { first-visit: stacks-block-height, total-visits: u0, total-weight-submitted: u0, 
         materials-submitted: (list), last-visit: u0, co2-impact: u0 }
       (map-get? user-location-usage { user: tx-sender, location-id: location-id }))))
    
    (asserts! (get active location) err-location-inactive)
    (asserts! (> weight-kg u0) err-invalid-input)
    
    ;; Update location statistics
    (map-set recycling-locations { location-id: location-id }
      (merge location {
        total-submissions: (+ (get total-submissions location) u1),
        total-weight-processed: (+ (get total-weight-processed location) weight-kg)
      }))
    
    ;; Update user location usage
    (map-set user-location-usage { user: tx-sender, location-id: location-id }
      (merge current-usage {
        total-visits: (+ (get total-visits current-usage) u1),
        total-weight-submitted: (+ (get total-weight-submitted current-usage) weight-kg),
        materials-submitted: (unwrap! (as-max-len? (append (get materials-submitted current-usage) material-type) u10) err-invalid-input),
        last-visit: stacks-block-height,
        co2-impact: (+ (get co2-impact current-usage) co2-saved)
      }))
    
    ;; Update user environmental impact
    (unwrap! (update-user-impact tx-sender co2-saved weight-kg) err-invalid-input)
    
    ;; Update location impact metrics
    (unwrap! (update-location-impact location-id co2-saved weight-kg) err-invalid-input)
    
    (ok co2-saved)
  )
)

(define-public (deactivate-location (location-id uint))
  (let ((location (unwrap! (map-get? recycling-locations { location-id: location-id }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    
    (map-set recycling-locations { location-id: location-id }
      (merge location { active: false }))
    
    (ok true)
  )
)

(define-public (update-location-capacity (location-id uint) (new-capacity uint))
  (let ((location (unwrap! (map-get? recycling-locations { location-id: location-id }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (and (>= new-capacity u1) (<= new-capacity u5)) err-invalid-input)
    
    (map-set recycling-locations { location-id: location-id }
      (merge location { capacity-rating: new-capacity }))
    
    (ok true)
  )
)

;; Private helper functions
(define-private (calculate-co2-impact (material-type (string-ascii 20)) (weight-kg uint))
  (if (is-eq material-type "plastic")
    (* weight-kg plastic-co2-saved)
    (if (is-eq material-type "glass")
      (* weight-kg glass-co2-saved)
      (if (is-eq material-type "metal")
        (* weight-kg metal-co2-saved)
        (if (is-eq material-type "paper")
          (* weight-kg paper-co2-saved)
          (if (is-eq material-type "electronics")
            (* weight-kg electronics-co2-saved)
            u0)))))
)

(define-private (update-user-impact (user principal) (co2-saved uint) (weight uint))
  (let ((current-impact (default-to 
          { total-co2-saved: u0, materials-diverted: u0, equivalent-trees-planted: u0, 
            impact-score: u0, last-calculated: u0 }
          (map-get? user-environmental-impact { user: user }))))
    
    (let ((new-co2-total (+ (get total-co2-saved current-impact) co2-saved))
          (new-weight-total (+ (get materials-diverted current-impact) weight))
          (trees-equivalent (/ new-co2-total u22000))) ;; 22kg CO2 per tree annually
      
      (map-set user-environmental-impact { user: user }
        {
          total-co2-saved: new-co2-total,
          materials-diverted: new-weight-total,
          equivalent-trees-planted: trees-equivalent,
          impact-score: (+ (* trees-equivalent u100) (/ new-weight-total u10)),
          last-calculated: stacks-block-height
        })
      
      ;; Update global stats
      (var-set global-co2-saved (+ (var-get global-co2-saved) co2-saved))
      (ok true)
    )
  )
)

(define-private (update-location-impact (location-id uint) (co2-saved uint) (weight uint))
  (let ((current-metrics (default-to 
          { total-co2-impact: u0, total-materials-processed: u0, efficiency-rating: u0, 
            environmental-score: u0, last-updated: u0 }
          (map-get? location-impact-metrics { location-id: location-id }))))
    
    (let ((new-co2-total (+ (get total-co2-impact current-metrics) co2-saved))
          (new-weight-total (+ (get total-materials-processed current-metrics) weight))
          (efficiency (if (> new-weight-total u0) (/ new-co2-total new-weight-total) u0)))
      
      (map-set location-impact-metrics { location-id: location-id }
        {
          total-co2-impact: new-co2-total,
          total-materials-processed: new-weight-total,
          efficiency-rating: efficiency,
          environmental-score: (+ (/ new-co2-total u1000) (/ new-weight-total u100)),
          last-updated: stacks-block-height
        })
      
      (ok true)
    )
  )
)
