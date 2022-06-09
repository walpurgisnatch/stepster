# Stepster
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
`(collect-from page 'div :attr 'class :test #'(lambda (node arg) (equal (ss:attribute node 'class) arg)) :test-args '("card"))`

### Extract functions

```
(extract-urls page test arg)

(extract-input-names page)

(extract-js-src page)

(extract-forms page)
```

### Download functions

(download-file "uri" "path/to/file")

(download-page "uri" "path/to/file")

(download-all-images "uri" "path/to/dir")

### Utilities functions

Return plump attribute from node
`(attribute node 'attr)`

Return string of text from all of the children nodes.
`(concat-node-text page)`

## Installation
Download stepster to your library directory and load it with quicklisp
`(ql:quickload :stepster)`

## License

Licensed under the MIT License.
