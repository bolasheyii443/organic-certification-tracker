;; organic-certification
;; Farm organic certification tracking and verification system

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-FARM-NOT-FOUND (err u101))
(define-constant ERR-CERTIFICATE-NOT-FOUND (err u102))
(define-constant ERR-CERTIFICATE-EXPIRED (err u103))
(define-constant ERR-INVALID-STATUS (err u104))

;; data maps and vars
(define-map farms 
  { farm-id: uint }
  { 
    owner: principal,
    name: (string-ascii 100),
    location: (string-ascii 200),
    size-acres: uint,
    registration-date: uint
  }
)

(define-map certifications
  { cert-id: uint }
  {
    farm-id: uint,
    inspector: principal,
    issue-date: uint,
    expiry-date: uint,
    status: (string-ascii 20),
    compliance-score: uint
  }
)

(define-map farm-products
  { farm-id: uint, product-id: uint }
  {
    product-name: (string-ascii 50),
    quantity: uint,
    harvest-date: uint,
    certified: bool
  }
)

(define-data-var farm-counter uint u0)
(define-data-var cert-counter uint u0)
(define-data-var product-counter uint u0)

;; private functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (is-farm-owner (farm-id uint))
  (match (map-get? farms { farm-id: farm-id })
    farm (is-eq tx-sender (get owner farm))
    false
  )
)

(define-private (is-certificate-valid (cert-id uint))
  (match (map-get? certifications { cert-id: cert-id })
    cert (and 
      (is-eq (get status cert) "active")
      (> (get expiry-date cert) burn-block-height)
    )
    false
  )
)

;; public functions
(define-public (register-farm (name (string-ascii 100)) (location (string-ascii 200)) (size-acres uint))
  (let ((new-farm-id (+ (var-get farm-counter) u1)))
    (map-set farms 
      { farm-id: new-farm-id }
      {
        owner: tx-sender,
        name: name,
        location: location,
        size-acres: size-acres,
        registration-date: burn-block-height
      }
    )
    (var-set farm-counter new-farm-id)
    (ok new-farm-id)
  )
)

(define-public (issue-certificate (farm-id uint) (expiry-date uint) (compliance-score uint))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? farms { farm-id: farm-id })) ERR-FARM-NOT-FOUND)
    (let ((new-cert-id (+ (var-get cert-counter) u1)))
      (map-set certifications
        { cert-id: new-cert-id }
        {
          farm-id: farm-id,
          inspector: tx-sender,
          issue-date: burn-block-height,
          expiry-date: expiry-date,
          status: "active",
          compliance-score: compliance-score
        }
      )
      (var-set cert-counter new-cert-id)
      (ok new-cert-id)
    )
  )
)

(define-public (add-product (farm-id uint) (product-name (string-ascii 50)) (quantity uint) (harvest-date uint))
  (begin
    (asserts! (is-farm-owner farm-id) ERR-NOT-AUTHORIZED)
    (let ((new-product-id (+ (var-get product-counter) u1)))
      (map-set farm-products
        { farm-id: farm-id, product-id: new-product-id }
        {
          product-name: product-name,
          quantity: quantity,
          harvest-date: harvest-date,
          certified: false
        }
      )
      (var-set product-counter new-product-id)
      (ok new-product-id)
    )
  )
)

(define-public (certify-product (farm-id uint) (product-id uint))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? farm-products { farm-id: farm-id, product-id: product-id })) ERR-FARM-NOT-FOUND)
    (match (map-get? farm-products { farm-id: farm-id, product-id: product-id })
      product (begin
        (map-set farm-products
          { farm-id: farm-id, product-id: product-id }
          (merge product { certified: true })
        )
        (ok true)
      )
      ERR-FARM-NOT-FOUND
    )
  )
)

(define-public (revoke-certificate (cert-id uint))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (match (map-get? certifications { cert-id: cert-id })
      cert (begin
        (map-set certifications
          { cert-id: cert-id }
          (merge cert { status: "revoked" })
        )
        (ok true)
      )
      ERR-CERTIFICATE-NOT-FOUND
    )
  )
)

;; read-only functions
(define-read-only (get-farm (farm-id uint))
  (map-get? farms { farm-id: farm-id })
)

(define-read-only (get-certificate (cert-id uint))
  (map-get? certifications { cert-id: cert-id })
)

(define-read-only (get-product (farm-id uint) (product-id uint))
  (map-get? farm-products { farm-id: farm-id, product-id: product-id })
)

(define-read-only (verify-organic-claim (farm-id uint))
  (is-some (map-get? farms { farm-id: farm-id }))
)
