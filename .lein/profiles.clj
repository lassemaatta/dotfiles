{:user           {:plugins      [[cider/cider-nrepl "LATEST"]]
                  :aliases      {"outdated" ["with-profile" "root-antq" "run" "-m" "antq.core" "--skip=pom"]}
                  :test-refresh {:quiet        true
                                 :changes-only true}}
 :root-antq      {:dependencies [[com.github.liquidz/antq "RELEASE"]]}

 :root-reveal    {:dependencies [[vlaaad/reveal "LATEST"]]
                  :repl-options {:nrepl-middleware [vlaaad.reveal.nrepl/middleware]}
                  :jvm-opts     ["-Dvlaaad.reveal.prefs={:font-size,16}"]}

 :root-benchmark {:dependencies [[criterium "0.4.6"]
                                 [com.clojure-goes-fast/clj-async-profiler "1.0.3"]]
                  :jvm-opts     ["-Djdk.attach.allowAttachSelf"]}

 :root-local-dev {:jvm-opts ["-XX:-OmitStackTraceInFastThrow"
                             "-Dclojure.main.report=stderr"]}

 :root-kondo     {:dependencies [[clj-kondo/clj-kondo "LATEST"]]}}
