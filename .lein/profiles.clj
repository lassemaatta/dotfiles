{:user           {:plugins      [[cider/cider-nrepl "LATEST"]
                                 [lein-collisions "LATEST"]
                                 [lein-cloverage "1.2.4"]]
                  :aliases      {"outdated" ["with-profile" "root-antq" "run" "-m" "antq.core" "--skip=pom" "--no-changes"]}
                  :test-refresh {:quiet        true
                                 :changes-only true}
                  :cloverage    {:lcov?  true
                                 :html?  false
                                 :output "."}}
 :root-antq      {:dependencies [[com.github.liquidz/antq "RELEASE"]
                                 [org.slf4j/slf4j-nop "RELEASE"]]}

 :root-reveal    {:dependencies [[vlaaad/reveal "LATEST"]]
                  :repl-options {:nrepl-middleware [vlaaad.reveal.nrepl/middleware]}
                  :jvm-opts     ["-Dvlaaad.reveal.prefs={:font-size,16}"]}

 :root-benchmark {:dependencies [[criterium "0.4.6"]
                                 [com.clojure-goes-fast/clj-async-profiler "1.6.2"]]
                  :jvm-opts     ["-Djdk.attach.allowAttachSelf"]}

 :root-reload    {:dependencies [[io.github.tonsky/clj-reload "0.7.0"]]}

 :root-local-dev {:jvm-opts ["-XX:-OmitStackTraceInFastThrow"
                             "-Dclojure.main.report=stderr"]}

 :root-kondo     {:dependencies [[clj-kondo/clj-kondo "LATEST"]]}}
