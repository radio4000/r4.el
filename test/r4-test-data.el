;;; r4-test-data.el --- Tests of r4: Data models, API parameters and columns  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with: make test

;;; Code:

(require 'r4-test-helpers)

(ert-deftest r4-test-track-from-json ()
  (let ((track (r4-track-from-json
                '((id . "abc") (slug . "oskar") (title . "Song") (media_id . "xyz")
                  (tags . ("a" "b")) (duration . 213) (created_at . "2026-10-18T12:13:17Z")))))
    (should (equal (r4-track-media-id track) "xyz"))
    (should (equal (r4-track-tags track) '("a" "b")))
    (should (equal (r4-track-web-url track) "https://radio4000.com/oskar/tracks/abc"))))

(ert-deftest r4-test-channel-from-json ()
  (let ((channel (r4-channel-from-json '((slug . "oskar") (track_count . 2648)))))
    (should (equal (r4-channel-track-count channel) 2648))
    (should (equal (r4-channel-web-url channel) "https://radio4000.com/oskar"))))

(ert-deftest r4-test-format ()
  (should (equal (r4-format-duration 213) "3:33"))
  (should (equal (r4-format-duration 3725) "1:02:05"))
  (should (equal (r4-format-duration 59.7) "0:59"))
  (should (equal (r4-format-duration nil) ""))
  (should (equal (r4-format-date "2026-10-18T12:13:17Z") "2026-10-18"))
  (should (equal (r4-one-line " a\n\tb ") "a b")))

(ert-deftest r4-test-api-params ()
  (should (equal (assoc "or" (r4-api--channels-params "jazz blues"))
                 '("or" "(fts.wfts.jazz blues,fts.fts.jazz:* & blues:*)")))
  (should-not (assoc "or" (r4-api--channels-params)))
  ;; Emacs versions escape different characters, so check the decoded
  ;; value, and that its & does not split the query.
  (let ((url (r4-api--url "x" (r4-api--channels-params "jazz blues"))))
    (should (string-match "[?&]or=\\([^&]*\\)" url))
    (should (equal (url-unhex-string (match-string 1 url))
                   "(fts.wfts.jazz blues,fts.fts.jazz:* & blues:*)"))))

(ert-deftest r4-test-search-filter ()
  (should (equal (r4-api-search-filter "osk") "(fts.wfts.osk,fts.fts.osk:*)"))
  ;; Web search operators disable the prefix search.
  (should (equal (r4-api-search-filter "jazz -blues") "(fts.wfts.jazz -blues)"))
  (should (equal (r4-api-search-filter "jazz or blues") "(fts.wfts.jazz or blues)"))
  ;; Characters breaking the PostgREST syntax are removed.
  (should (equal (r4-api-search-filter "a,b(c)") "(fts.wfts.a b c,fts.fts.a:* & b:* & c:*)"))
  (should-not (r4-api-search-filter " , ")))

(ert-deftest r4-test-search-parse ()
  (should (equal (r4-search-parse "@ko002 #Jazz miles  davis @Oskar")
                 '(:slugs ("ko002" "oskar") :tags ("jazz") :query "miles davis")))
  (should-not (r4-search-parse "  "))
  (should (equal (r4-api--tracks-params '(:slugs ("a" "b") :tags ("jazz" "dub") :mention "x"))
                 `(("select" ,r4-api--track-columns)
                   ("slug" "in.(a,b)")
                   ("tags" "ov.{\"jazz\",\"dub\"}")
                   ("description" "ilike.*@x*")
                   ("order" "created_at.desc")
                   ("limit" "5000")))))

(ert-deftest r4-test-columns-sort ()
  (let* ((specs r4-channels-column-specs)
         (format (r4-columns-list-format '(slug tracks) specs))
         (sort-tracks (nth 2 (aref format 1)))
         (entry (lambda (count) (list (r4-channel-create :slug "x" :track-count count) nil))))
    ;; Numbers sort by value, not by text.
    (should (funcall sort-tracks (funcall entry 9) (funcall entry 10)))
    (should-not (funcall sort-tracks (funcall entry 10) (funcall entry 9)))
    (should (funcall sort-tracks (funcall entry nil) (funcall entry 0)))
    ;; The last column takes the remaining width.
    (should (equal (nth 1 (aref format 1)) 0))
    (should (r4-columns-less-p "abc" "ABD"))
    (should (equal (r4-columns-valid '(slug nope tracks) specs) '(slug tracks)))))

(ert-deftest r4-test-channel-image-url ()
  (should (equal (r4-channel-image-url (r4-channel-create :image "abc") 100)
                 "https://res.cloudinary.com/radio4000/image/upload/w_100,h_100,c_thumb,q_80/abc.jpg"))
  (should-not (r4-channel-image-url (r4-channel-create :image "")))
  (should-not (r4-channel-image-url (r4-channel-create))))

(provide 'r4-test-data)

;;; r4-test-data.el ends here
