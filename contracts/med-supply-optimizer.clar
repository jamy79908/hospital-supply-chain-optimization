;; Medical Supply Chain Optimizer
;; Hospital inventory management and supply sharing platform

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_SUPPLY_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_QUANTITY (err u102))
(define-constant ERR_INVALID_HOSPITAL (err u103))
(define-constant ERR_EXPIRED_SUPPLY (err u104))
(define-constant ERR_INVALID_REQUEST (err u105))

;; Supply Status Constants
(define-constant STATUS_AVAILABLE u1)
(define-constant STATUS_LOW_STOCK u2)
(define-constant STATUS_OUT_OF_STOCK u3)
(define-constant STATUS_EXPIRED u4)
(define-constant STATUS_RECALLED u5)

;; Emergency Levels
(define-constant EMERGENCY_NORMAL u1)
(define-constant EMERGENCY_LOW u2)
(define-constant EMERGENCY_CRITICAL u3)
(define-constant EMERGENCY_CRISIS u4)

;; Data Variables
(define-data-var supply-id-counter uint u0)
(define-data-var hospital-id-counter uint u0)
(define-data-var transfer-id-counter uint u0)
(define-data-var total-supplies-managed uint u0)
(define-data-var total-waste-prevented uint u0)
(define-data-var emergency-threshold uint u10) ;; 10% of normal stock
(define-data-var reorder-threshold uint u20) ;; 20% of normal stock

;; Data Maps
(define-map hospitals
    { hospital-id: uint }
    {
        name: (string-ascii 100),
        location: (string-ascii 100),
        bed-capacity: uint,
        emergency-level: uint,
        active: bool,
        admin: principal,
        total-supplies: uint,
        last-updated: uint
    }
)

(define-map supplies
    { supply-id: uint }
    {
        hospital-id: uint,
        supply-name: (string-ascii 100),
        category: (string-ascii 50),
        current-quantity: uint,
        minimum-quantity: uint,
        maximum-quantity: uint,
        unit-cost: uint,
        expiration-date: uint,
        lot-number: (string-ascii 50),
        status: uint,
        last-updated: uint
    }
)

(define-map supply-transfers
    { transfer-id: uint }
    {
        from-hospital: uint,
        to-hospital: uint,
        supply-id: uint,
        quantity: uint,
        reason: (string-ascii 100),
        status: uint,
        requested-at: uint,
        completed-at: uint
    }
)

(define-map hospital-admins
    { admin: principal }
    {
        hospital-id: uint,
        permissions: uint,
        active: bool
    }
)

(define-map supply-usage
    { hospital-id: uint, supply-name: (string-ascii 100) }
    {
        daily-usage: uint,
        weekly-usage: uint,
        monthly-usage: uint,
        last-calculated: uint
    }
)

(define-map emergency-requests
    { request-id: uint }
    {
        requesting-hospital: uint,
        supply-needed: (string-ascii 100),
        quantity-needed: uint,
        urgency-level: uint,
        fulfilled: bool,
        created-at: uint
    }
)

;; Private Functions
(define-private (calculate-reorder-quantity (supply-id uint))
    (let (
        (supply-data (unwrap! (map-get? supplies { supply-id: supply-id }) u0))
        (hospital-id (get hospital-id supply-data))
        (usage-data (default-to 
            { daily-usage: u5, weekly-usage: u35, monthly-usage: u150, last-calculated: u0 }
            (map-get? supply-usage { hospital-id: hospital-id, supply-name: (get supply-name supply-data) })
        ))
        (target-quantity (get maximum-quantity supply-data))
        (current-quantity (get current-quantity supply-data))
    )
    (if (> target-quantity current-quantity)
        (- target-quantity current-quantity)
        u0
    )
    )
)

(define-private (update-supply-status (supply-id uint))
    (let (
        (supply-data (unwrap! (map-get? supplies { supply-id: supply-id }) false))
        (current-qty (get current-quantity supply-data))
        (min-qty (get minimum-quantity supply-data))
        (emergency-qty (/ (* min-qty (var-get emergency-threshold)) u100))
        (reorder-qty (/ (* min-qty (var-get reorder-threshold)) u100))
        (new-status (if (is-eq current-qty u0)
            STATUS_OUT_OF_STOCK
            (if (<= current-qty emergency-qty)
                STATUS_LOW_STOCK
                STATUS_AVAILABLE
            )
        ))
    )
    (map-set supplies { supply-id: supply-id }
        (merge supply-data { status: new-status, last-updated: block-height })
    )
    true
    )
)

(define-private (find-available-supply (supply-name (string-ascii 100)) (excluding-hospital uint) (quantity-needed uint))
    ;; Simplified search - in real implementation would iterate through hospitals
    (let (
        (hospital-count (var-get hospital-id-counter))
    )
    ;; Return first hospital ID that might have supplies (simplified)
    (if (> hospital-count u1) u1 u0)
    )
)

;; Public Functions
(define-public (register-hospital (name (string-ascii 100)) (location (string-ascii 100)) (bed-capacity uint) (admin principal))
    (let (
        (new-hospital-id (+ (var-get hospital-id-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> bed-capacity u0) ERR_INVALID_REQUEST)
    
    (map-set hospitals { hospital-id: new-hospital-id }
        {
            name: name,
            location: location,
            bed-capacity: bed-capacity,
            emergency-level: EMERGENCY_NORMAL,
            active: true,
            admin: admin,
            total-supplies: u0,
            last-updated: block-height
        }
    )
    
    (map-set hospital-admins { admin: admin }
        {
            hospital-id: new-hospital-id,
            permissions: u100, ;; Full permissions
            active: true
        }
    )
    
    (var-set hospital-id-counter new-hospital-id)
    (ok new-hospital-id)
    )
)

(define-public (add-supply (hospital-id uint) (supply-name (string-ascii 100)) (category (string-ascii 50))
                          (quantity uint) (min-qty uint) (max-qty uint) (unit-cost uint) 
                          (expiration-date uint) (lot-number (string-ascii 50)))
    (let (
        (new-supply-id (+ (var-get supply-id-counter) u1))
        (hospital-data (unwrap! (map-get? hospitals { hospital-id: hospital-id }) ERR_INVALID_HOSPITAL))
        (admin-data (unwrap! (map-get? hospital-admins { admin: tx-sender }) ERR_NOT_AUTHORIZED))
    )
    (asserts! (is-eq (get hospital-id admin-data) hospital-id) ERR_NOT_AUTHORIZED)
    (asserts! (get active admin-data) ERR_NOT_AUTHORIZED)
    (asserts! (> quantity u0) ERR_INVALID_REQUEST)
    
    (map-set supplies { supply-id: new-supply-id }
        {
            hospital-id: hospital-id,
            supply-name: supply-name,
            category: category,
            current-quantity: quantity,
            minimum-quantity: min-qty,
            maximum-quantity: max-qty,
            unit-cost: unit-cost,
            expiration-date: expiration-date,
            lot-number: lot-number,
            status: STATUS_AVAILABLE,
            last-updated: block-height
        }
    )
    
    ;; Update hospital total supplies
    (map-set hospitals { hospital-id: hospital-id }
        (merge hospital-data { 
            total-supplies: (+ (get total-supplies hospital-data) u1),
            last-updated: block-height
        })
    )
    
    (var-set supply-id-counter new-supply-id)
    (var-set total-supplies-managed (+ (var-get total-supplies-managed) u1))
    (update-supply-status new-supply-id)
    (ok new-supply-id)
    )
)

(define-public (update-supply-quantity (supply-id uint) (new-quantity uint))
    (let (
        (supply-data (unwrap! (map-get? supplies { supply-id: supply-id }) ERR_SUPPLY_NOT_FOUND))
        (admin-data (unwrap! (map-get? hospital-admins { admin: tx-sender }) ERR_NOT_AUTHORIZED))
    )
    (asserts! (is-eq (get hospital-id admin-data) (get hospital-id supply-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get active admin-data) ERR_NOT_AUTHORIZED)
    
    (map-set supplies { supply-id: supply-id }
        (merge supply-data { 
            current-quantity: new-quantity,
            last-updated: block-height
        })
    )
    
    (update-supply-status supply-id)
    (ok true)
    )
)

(define-public (request-emergency-supply (supply-name (string-ascii 100)) (quantity-needed uint) (urgency-level uint))
    (let (
        (admin-data (unwrap! (map-get? hospital-admins { admin: tx-sender }) ERR_NOT_AUTHORIZED))
        (requesting-hospital (get hospital-id admin-data))
        (new-request-id (+ (var-get transfer-id-counter) u1))
    )
    (asserts! (get active admin-data) ERR_NOT_AUTHORIZED)
    (asserts! (> quantity-needed u0) ERR_INVALID_REQUEST)
    (asserts! (<= urgency-level EMERGENCY_CRISIS) ERR_INVALID_REQUEST)
    
    (map-set emergency-requests { request-id: new-request-id }
        {
            requesting-hospital: requesting-hospital,
            supply-needed: supply-name,
            quantity-needed: quantity-needed,
            urgency-level: urgency-level,
            fulfilled: false,
            created-at: block-height
        }
    )
    
    (var-set transfer-id-counter new-request-id)
    (ok new-request-id)
    )
)

(define-public (fulfill-supply-request (request-id uint) (supplying-hospital uint) (supply-id uint) (quantity uint))
    (let (
        (request-data (unwrap! (map-get? emergency-requests { request-id: request-id }) ERR_INVALID_REQUEST))
        (supply-data (unwrap! (map-get? supplies { supply-id: supply-id }) ERR_SUPPLY_NOT_FOUND))
        (admin-data (unwrap! (map-get? hospital-admins { admin: tx-sender }) ERR_NOT_AUTHORIZED))
    )
    (asserts! (is-eq (get hospital-id admin-data) supplying-hospital) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get hospital-id supply-data) supplying-hospital) ERR_NOT_AUTHORIZED)
    (asserts! (>= (get current-quantity supply-data) quantity) ERR_INSUFFICIENT_QUANTITY)
    (asserts! (not (get fulfilled request-data)) ERR_INVALID_REQUEST)
    
    ;; Update supply quantity
    (map-set supplies { supply-id: supply-id }
        (merge supply-data { 
            current-quantity: (- (get current-quantity supply-data) quantity),
            last-updated: block-height
        })
    )
    
    ;; Mark request as fulfilled
    (map-set emergency-requests { request-id: request-id }
        (merge request-data { fulfilled: true })
    )
    
    ;; Create transfer record
    (let ((new-transfer-id (+ (var-get transfer-id-counter) u1)))
        (map-set supply-transfers { transfer-id: new-transfer-id }
            {
                from-hospital: supplying-hospital,
                to-hospital: (get requesting-hospital request-data),
                supply-id: supply-id,
                quantity: quantity,
                reason: "Emergency supply transfer",
                status: u1, ;; Completed
                requested-at: (get created-at request-data),
                completed-at: block-height
            }
        )
        (var-set transfer-id-counter new-transfer-id)
    )
    
    (update-supply-status supply-id)
    (ok true)
    )
)

(define-public (set-emergency-level (hospital-id uint) (emergency-level uint))
    (let (
        (hospital-data (unwrap! (map-get? hospitals { hospital-id: hospital-id }) ERR_INVALID_HOSPITAL))
        (admin-data (unwrap! (map-get? hospital-admins { admin: tx-sender }) ERR_NOT_AUTHORIZED))
    )
    (asserts! (is-eq (get hospital-id admin-data) hospital-id) ERR_NOT_AUTHORIZED)
    (asserts! (<= emergency-level EMERGENCY_CRISIS) ERR_INVALID_REQUEST)
    
    (map-set hospitals { hospital-id: hospital-id }
        (merge hospital-data { 
            emergency-level: emergency-level,
            last-updated: block-height
        })
    )
    
    (ok true)
    )
)

;; Read-only functions
(define-read-only (get-hospital-info (hospital-id uint))
    (map-get? hospitals { hospital-id: hospital-id })
)

(define-read-only (get-supply-details (supply-id uint))
    (map-get? supplies { supply-id: supply-id })
)

(define-read-only (get-supply-status (supply-id uint))
    (match (map-get? supplies { supply-id: supply-id })
        supply-data (ok (get status supply-data))
        ERR_SUPPLY_NOT_FOUND
    )
)

(define-read-only (get-platform-stats)
    (ok {
        total-hospitals: (var-get hospital-id-counter),
        total-supplies: (var-get total-supplies-managed),
        total-transfers: (var-get transfer-id-counter),
        waste-prevented: (var-get total-waste-prevented)
    })
)

(define-read-only (get-hospital-emergency-level (hospital-id uint))
    (match (map-get? hospitals { hospital-id: hospital-id })
        hospital-data (ok (get emergency-level hospital-data))
        ERR_INVALID_HOSPITAL
    )
)

(define-read-only (check-reorder-needed (supply-id uint))
    (match (map-get? supplies { supply-id: supply-id })
        supply-data (let (
            (current-qty (get current-quantity supply-data))
            (reorder-qty (/ (* (get minimum-quantity supply-data) (var-get reorder-threshold)) u100))
        )
        (ok (<= current-qty reorder-qty))
        )
        ERR_SUPPLY_NOT_FOUND
    )
)
