((text) @injection.content
    (#not-has-ancestor? @injection.content "envoy")
    (#set! injection.combined)
    (#set! injection.language php))
;; extends

; AlpineJS attributes
; (attribute
;   (attribute_name) @_attr
;     (#lua-match? @_attr "^x%-%l")
;   (quoted_attribute_value
;     (attribute_value) @injection.content)
;   (#set! injection.language "javascript"))
