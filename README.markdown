# Stepster
[![Quicklisp](http://quickdocs.org/badge/stepster.svg)](http://quickdocs.org/stepster/)  
  
Parsing library

## Usage

Parse main node  
`(setf page (parse "uri"))`

### Collect stuff from page

To collect all div elements  
`(collect-from page 'div)`

To collect all links  
`(collect-from page 'a :attr 'href)`

Or simply  
`(extract-urls page)`

Also it is possible to set up filter  
`(collect-from page 'a :attr 'href :test #'same-domain :test-args "uri")`

All divs with class "card"  
`(collect-from page 'div :test #'(lambda (node arg) (equal (ss:attribute node 'class) arg)) :test-args '"card")`

### Extract functions

Get all links from page  
`(extract-urls page test arg)`

All input names  
`(extract-input-names page)`

Js sources  
`(extract-js-src page)`

Forms  
`(extract-forms page)`

### Download functions

`(download-file "uri" "path/to/file")`

`(download-page "uri" "path/to/file")`

`(download-all-images "uri" "path/to/dir")`

### Json works

Recursively parse value of one element from json  
`(ss:jfinder json "value")`

Also possible to set full path or part of the path to element  
`(ss:jfinder json '("page" "engine_version" "value"))`

`(ss:jfinder json '("engine_version" "value"))`

Recursively parse list of elements values from array  
```
CL-USER> json ;; may also be a string
(:|infiniteSearchResult|
 ((:|itemId| 276843344 :|modelVersion|
   "ml_recommendations_engine.recsys_lightfm_desc_mixed.v1" :|score|
   0.05633471119569102d0)
  (:|itemId| 271451418 :|modelVersion|
   "ml_recommendations_engine.recsys_lightfm_desc_mixed.v1" :|score|
   0.05633471119569102d0)
  (:|itemId| 274323904 :|modelVersion|
   "ml_recommendations_engine.recsys_lightfm_desc_mixed.v1" :|score|
   0.05633471119569102d0)))
CL-USER> (ss:collect-json json '("itemId" "score"))
((276843344 0.05633471119569102d0) (271451418 0.05633471119569102d0)
 (274323904 0.05633471119569102d0))
```

Create json back with this values with  
```
CL-USER> (ss:pack-with '("Id" "value") (ss:collect-json json '("itemId" "score")))
(("Id" 276843344 "value" 0.05633471119569102d0)
 ("Id" 271451418 "value" 0.05633471119569102d0)
 ("Id" 274323904 "value" 0.05633471119569102d0))
CL-USER> (ss:pack-to-json '("Id" "value") (ss:collect-json json '("itemId" "score")))
"[{\"Id\":276843344,\"value\":0.05633471119569102},{\"Id\":271451418,\"value\":0.05633471119569102},{\"Id\":274323904,\"value\":0.05633471119569102}]"
```

### Utilities functions

Return plump attribute from node  
`(attribute node 'attr)`

Return text from all of the children nodes.  
`(concat-node-text page)`

## Installation
### Quicklisp.  
`(ql:quickload :stepster)`

## License

Licensed under the MIT License.
