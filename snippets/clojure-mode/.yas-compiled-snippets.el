;;; Compiled snippets and support files for `clojure-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'clojure-mode
										 '(("<time"
												"(comment\n  (defn add\n    \"Add the given amount of units to the timestamp\n   Usage: (add 1630925574504 :hours 10)\"\n    [ts unit value]\n    (-> ^java.time.Instant (java.time.Instant/ofEpochMilli ts)\n      (.plus ^Long value ^java.time.temporal.TemporalUnit\n        (get chrono-units unit))\n      .toEpochMilli))\n\n  (defn subtract\n    \"Subtract the given amount of units from the timestamp\n   Usage: (subtract 1630925574504 :days 100)\"\n    [ts unit value]\n    (-> ^java.time.Instant (java.time.Instant/ofEpochMilli ts)\n      (.minus ^Long value ^java.time.temporal.TemporalUnit\n        (get chrono-units unit))\n      .toEpochMilli))\n\n  (defn seconds-to-format\n    \"Converts time in seconds to format like 'yyyy-MM-dd', given timezone.\"\n    ([seconds format-str]\n     (seconds-to-format seconds format-str \"UTC\"))\n\n    ([seconds format-str timezone]\n     (.format (java.time.format.DateTimeFormatter/ofPattern format-str)\n       (-> (java.time.Instant/ofEpochSecond seconds)\n         (java.time.ZonedDateTime/ofInstant (java.time.ZoneId/of timezone)))))))"
												"timestamps code, mostly Java interop" nil nil nil
												"/home/german/.emacs.d/snippets/clojure-mode/timestamp"
												nil nil)
											 ("<io"
												"(comment\n  (def tmp-dir\n    (.toString\n      (java.nio.file.Files/createTempDirectory \"some-prefix-\"\n        (into-array java.nio.file.attribute.FileAttribute []))))\n\n  (def temp-excel-file (java.io.File/createTempFile \"temp-\" \".xlsx\"))\n\n  (System/getProperty \"java.io.tmpdir\")\n\n  (def jpeg-files\n    (->> tmp-dir java.io.File. file-seq (drop 1)\n      (sort-by #(.getName %))))\n\n  (with-open [out (jio/output-stream temp-excel-file)]\n    (.write out (byte-array 0)))\n\n  (with-open [in (jio/input-stream temp-excel-file)]\n                  (.readAllBytes in))\n\n  (-> \"some-file\"\n    clojure.java.io/resource\n    .toURI\n    java.io.File.\n    clojure.java.io/input-stream)\n\n  (-> (Thread/currentThread)\n    .getContextClassLoader\n    (.getResources \"\")\n    enumeration-seq))"
												"Java file input / output" nil nil nil
												"/home/german/.emacs.d/snippets/clojure-mode/io" nil nil)
											 ("<intro"
												"(comment\n	(require '[playback.preload])\n\n  (do\n    (require '[portal.api :as p])\n    (def p (portal.api/open))\n		; Add portal as a tap> target\n    (add-tap #'portal.api/submit)) $0\n\n  (tap> :hello) ; Start tapping out values\n  (portal.api/clear) ; Clear all values\n  (tap> :world) ; Tap out more values\n  (prn @p) ; bring selected value back into repl\n\n  (remove-tap #'portal.api/submit) ; Remove portal from tap> targetset\n  (portal.api/close) ; Close the inspector when done\n  )\n"
												"introspection tools" nil nil nil
												"/home/german/.emacs.d/snippets/clojure-mode/introspection"
												nil nil)))


;;; Do not edit! File generated at Mon Dec 15 12:25:59 2025
