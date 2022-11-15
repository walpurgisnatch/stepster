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

### Utilities functions

Return plump attribute from node  
`(attribute node 'attr)`

Return string of text from all of the children nodes.  
`(concat-node-text page)`

## Installation
### Quicklisp.  
`(ql:quickload :stepster)`

## License

Licensed under the MIT License.
