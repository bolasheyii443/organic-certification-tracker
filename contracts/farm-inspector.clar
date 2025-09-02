;; farm-inspector
;; Inspector scheduling and compliance monitoring system

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-INSPECTOR-NOT-FOUND (err u201))
(define-constant ERR-INSPECTION-NOT-FOUND (err u202))
(define-constant ERR-INVALID-DATE (err u203))
(define-constant ERR-ALREADY-COMPLETED (err u204))

;; data maps and vars
(define-map inspectors
  { inspector-id: principal }
  {
    name: (string-ascii 100),
    certification-level: (string-ascii 20),
    active: bool,
    registration-date: uint
  }
)

(define-map inspections
  { inspection-id: uint }
  {
    farm-id: uint,
    inspector: principal,
    scheduled-date: uint,
    completed-date: (optional uint),
    status: (string-ascii 20),
    compliance-score: (optional uint),
    notes: (optional (string-ascii 500))
  }
)

(define-map compliance-history
  { farm-id: uint, inspection-id: uint }
  {
    previous-score: uint,
    current-score: uint,
    improvement: bool,
    violations: uint
  }
)

(define-data-var inspection-counter uint u0)

;; private functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (is-registered-inspector (inspector principal))
  (match (map-get? inspectors { inspector-id: inspector })
    inspector-data (get active inspector-data)
    false
  )
)

(define-private (is-inspection-owner (inspection-id uint))
  (match (map-get? inspections { inspection-id: inspection-id })
    inspection (is-eq tx-sender (get inspector inspection))
    false
  )
)

;; public functions
(define-public (register-inspector (inspector principal) (name (string-ascii 100)) (cert-level (string-ascii 20)))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (map-set inspectors
      { inspector-id: inspector }
      {
        name: name,
        certification-level: cert-level,
        active: true,
        registration-date: burn-block-height
      }
    )
    (ok true)
  )
)

(define-public (schedule-inspection (farm-id uint) (inspector principal) (scheduled-date uint))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (is-registered-inspector inspector) ERR-INSPECTOR-NOT-FOUND)
    (asserts! (> scheduled-date burn-block-height) ERR-INVALID-DATE)
    (let ((new-inspection-id (+ (var-get inspection-counter) u1)))
      (map-set inspections
        { inspection-id: new-inspection-id }
        {
          farm-id: farm-id,
          inspector: inspector,
          scheduled-date: scheduled-date,
          completed-date: none,
          status: "scheduled",
          compliance-score: none,
          notes: none
        }
      )
      (var-set inspection-counter new-inspection-id)
      (ok new-inspection-id)
    )
  )
)

(define-public (complete-inspection (inspection-id uint) (compliance-score uint) (notes (string-ascii 500)))
  (begin
    (asserts! (is-inspection-owner inspection-id) ERR-NOT-AUTHORIZED)
    (match (map-get? inspections { inspection-id: inspection-id })
      inspection (begin
        (asserts! (is-eq (get status inspection) "scheduled") ERR-ALREADY-COMPLETED)
        (map-set inspections
          { inspection-id: inspection-id }
          (merge inspection {
            completed-date: (some burn-block-height),
            status: "completed",
            compliance-score: (some compliance-score),
            notes: (some notes)
          })
        )
        ;; Record compliance history
        (map-set compliance-history
          { farm-id: (get farm-id inspection), inspection-id: inspection-id }
          {
            previous-score: u0, ;; Could be enhanced to track actual previous score
            current-score: compliance-score,
            improvement: (>= compliance-score u80), ;; Assuming 80+ is good
            violations: (if (< compliance-score u60) u1 u0)
          }
        )
        (ok true)
      )
      ERR-INSPECTION-NOT-FOUND
    )
  )
)

(define-public (update-inspection-status (inspection-id uint) (new-status (string-ascii 20)))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (match (map-get? inspections { inspection-id: inspection-id })
      inspection (begin
        (map-set inspections
          { inspection-id: inspection-id }
          (merge inspection { status: new-status })
        )
        (ok true)
      )
      ERR-INSPECTION-NOT-FOUND
    )
  )
)

(define-public (deactivate-inspector (inspector principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (match (map-get? inspectors { inspector-id: inspector })
      inspector-data (begin
        (map-set inspectors
          { inspector-id: inspector }
          (merge inspector-data { active: false })
        )
        (ok true)
      )
      ERR-INSPECTOR-NOT-FOUND
    )
  )
)

;; read-only functions
(define-read-only (get-inspector (inspector principal))
  (map-get? inspectors { inspector-id: inspector })
)

(define-read-only (get-inspection (inspection-id uint))
  (map-get? inspections { inspection-id: inspection-id })
)

(define-read-only (get-compliance-history (farm-id uint) (inspection-id uint))
  (map-get? compliance-history { farm-id: farm-id, inspection-id: inspection-id })
)

(define-read-only (is-farm-compliant (farm-id uint))
  ;; Simple compliance check - could be enhanced with more sophisticated logic
  (match (get-latest-inspection-score farm-id)
    score (>= score u70)
    false
  )
)

(define-private (get-latest-inspection-score (farm-id uint))
  ;; Simplified - in a real implementation, you'd iterate through inspections
  ;; to find the most recent one for this farm
  (match (map-get? inspections { inspection-id: u1 })
    inspection (if (is-eq (get farm-id inspection) farm-id)
      (get compliance-score inspection)
      none
    )
    none
  )
)

(define-read-only (get-inspector-workload (inspector principal))
  ;; Returns count of scheduled inspections for an inspector
  ;; Simplified implementation - real version would iterate through all inspections
  u0 ;; Placeholder
)
