#!/bin/bash
# Project definitions for compile time benchmark
# Format: "url|size_category|build_system|notes"

# =============================================================================
# FORTRAN PROJECTS (30) - mostly fpm, good success rate
# =============================================================================

FORTRAN_PROJECTS=(
    # Small (<2k LOC)
    "https://github.com/wavebitscientific/functional-fortran|small|fpm|functional programming"
    "https://github.com/jacobwilliams/fortran-csv-module|small|fpm|CSV reader"
    "https://github.com/wavebitscientific/datetime-fortran|small|fpm|datetime handling"
    "https://github.com/jacobwilliams/json-fortran|small|fpm|JSON parser"
    "https://github.com/interkosmos/fortran-curl|small|fpm|libcurl bindings"
    "https://github.com/perazz/fortran-regex|small|fpm|regex library"
    "https://github.com/interkosmos/fortran-zlib|small|fpm|compression bindings"
    "https://github.com/fortran-lang/test-drive|small|fpm|unit testing"
    "https://github.com/perazz/fortran-bessels|small|fpm|Bessel functions"
    "https://github.com/jacobwilliams/fortran-colors|small|fpm|terminal colors"

    # Medium (2k-20k LOC)
    "https://github.com/fortran-lang/stdlib|medium|fpm|standard library"
    "https://github.com/modern-fortran/neural-fortran|medium|fpm|neural networks"
    "https://github.com/fortran-lang/fpm|medium|fpm|package manager"
    "https://github.com/jacobwilliams/pyplot-fortran|medium|fpm|matplotlib bindings"
    "https://github.com/jacobwilliams/quadpack|medium|fpm|numerical integration"
    "https://github.com/jacobwilliams/bspline-fortran|medium|fpm|B-spline interpolation"
    "https://github.com/jacobwilliams/dop853|medium|fpm|ODE solver"
    "https://github.com/jacobwilliams/roots-fortran|medium|fpm|root finding"
    "https://github.com/jacobwilliams/geodesic-fortran|medium|fpm|geodesic calculations"
    "https://github.com/jacobwilliams/daglib|medium|fpm|DAG sorting"

    # Large (20k-100k LOC)
    "https://github.com/jacobwilliams/Fortran-Astrodynamics-Toolkit|large|fpm|astrodynamics"
    "https://github.com/Reference-LAPACK/lapack|large|cmake|linear algebra"
    "https://github.com/jacobwilliams/NumDiff|large|fpm|numerical diff"
    "https://github.com/jacobwilliams/slsqp|large|fpm|optimization"
    "https://github.com/jacobwilliams/polyroots-fortran|large|fpm|polynomial roots"
    "https://github.com/jacobwilliams/fortran-search-and-sort|large|fpm|algorithms"
    "https://github.com/jacobwilliams/finterp|large|fpm|interpolation"
    "https://github.com/jacobwilliams/LBFGS|large|fpm|optimization"
    "https://github.com/jacobwilliams/rklib|large|fpm|Runge-Kutta"
    "https://github.com/jacobwilliams/argv-fortran|large|fpm|CLI arguments"
)

# =============================================================================
# C PROJECTS (30) - cmake/make, header-only
# =============================================================================

C_PROJECTS=(
    # Small (<2k LOC)
    "https://github.com/zserge/jsmn|small|make|JSON tokenizer"
    "https://github.com/nothings/stb|small|header|single-header libs"
    "https://github.com/rxi/log.c|small|make|logging"
    "https://github.com/sheredom/utf8.h|small|header|UTF-8 handling"
    "https://github.com/antirez/sds|small|make|dynamic strings"
    "https://github.com/troydhanson/uthash|small|header|hash tables"
    "https://github.com/kgabis/parson|small|make|JSON parser"
    "https://github.com/benhoyt/inih|small|make|INI parser"
    "https://github.com/clibs/buffer|small|make|buffer library"
    "https://github.com/clibs/list|small|make|linked list"

    # Medium (2k-20k LOC)
    "https://github.com/DaveGamble/cJSON|medium|cmake|JSON parser"
    "https://github.com/madler/zlib|medium|cmake|compression"
    "https://github.com/glennrp/libpng|medium|cmake|PNG handling"
    "https://github.com/akheron/jansson|medium|cmake|JSON library"
    "https://github.com/argtable/argtable3|medium|cmake|CLI parsing"
    "https://github.com/ThrowTheSwitch/Unity|medium|cmake|unit testing"
    "https://github.com/B-Con/crypto-algorithms|medium|make|crypto primitives"
    "https://github.com/attractivechaos/klib|medium|header|generic library"
    "https://github.com/ianlancetaylor/libbacktrace|medium|cmake|backtraces"
    "https://github.com/antirez/linenoise|medium|make|line editing"

    # Large (20k-100k LOC)
    "https://github.com/lua/lua|large|make|scripting language"
    "https://github.com/rui314/chibicc|large|make|C compiler"
    "https://github.com/lz4/lz4|large|cmake|compression"
    "https://github.com/facebook/zstd|large|cmake|compression"
    "https://github.com/redis/hiredis|large|cmake|Redis client"
    "https://github.com/libevent/libevent|large|cmake|event library"
    "https://github.com/libuv/libuv|large|cmake|async I/O"
    "https://github.com/sqlite/sqlite|large|make|database"
    "https://github.com/rgerhards/libfastjson|large|cmake|fast JSON"
    "https://github.com/json-c/json-c|large|cmake|JSON library"
)

# =============================================================================
# C++ PROJECTS (35) - focus on header-only and simple cmake
# =============================================================================

CPP_PROJECTS=(
    # Small - header-only (high success rate)
    "https://github.com/nlohmann/json|small|cmake|JSON header-only"
    "https://github.com/catchorg/Catch2|small|cmake|testing"
    "https://github.com/doctest/doctest|small|cmake|testing"
    "https://github.com/jarro2783/cxxopts|small|cmake|CLI parsing"
    "https://github.com/Neargye/magic_enum|small|cmake|enum reflection"
    "https://github.com/marzer/tomlplusplus|small|cmake|TOML parser"
    "https://github.com/p-ranav/indicators|small|cmake|progress bars"
    "https://github.com/CLIUtils/CLI11|small|cmake|CLI parsing"
    "https://github.com/p-ranav/argparse|small|cmake|argument parsing"
    "https://github.com/p-ranav/tabulate|small|cmake|table formatting"
    "https://github.com/fmtlib/fmt|small|cmake|formatting"
    "https://github.com/ericniebler/range-v3|small|cmake|ranges"

    # Medium - simple cmake projects
    "https://github.com/Tencent/rapidjson|medium|cmake|JSON"
    "https://github.com/zeux/pugixml|medium|cmake|XML"
    "https://github.com/jbeder/yaml-cpp|medium|cmake|YAML"
    "https://github.com/google/googletest|medium|cmake|testing"
    "https://github.com/google/benchmark|medium|cmake|microbenchmarks"
    "https://github.com/yhirose/cpp-httplib|medium|cmake|HTTP server"
    "https://github.com/HowardHinnant/date|medium|cmake|datetime"
    "https://github.com/gabime/spdlog|medium|cmake|logging"
    "https://github.com/cameron314/concurrentqueue|medium|header|lock-free queue"
    "https://github.com/taskflow/taskflow|medium|cmake|parallel tasks"
    "https://github.com/onqtam/doctest|medium|cmake|testing"
    "https://github.com/bombela/backward-cpp|medium|cmake|stack traces"

    # Large - well-maintained projects
    "https://github.com/simdjson/simdjson|large|cmake|JSON parser"
    "https://github.com/google/re2|large|cmake|regex engine"
    "https://github.com/google/snappy|large|cmake|compression"
    "https://github.com/google/flatbuffers|large|cmake|serialization"
    "https://github.com/msgpack/msgpack-c|large|cmake|serialization"
    "https://github.com/USCiLab/cereal|large|cmake|serialization"
    "https://github.com/xtensor-stack/xtensor|large|cmake|tensors"
    "https://github.com/ArthurSonzogni/FTXUI|large|cmake|TUI library"
    "https://github.com/foonathan/memory|large|cmake|allocators"
    "https://github.com/pantor/inja|large|cmake|templates"
    "https://github.com/martinus/robin-hood-hashing|large|cmake|hash map"
)

# =============================================================================
# ZIG PROJECTS (35) - focus on projects with working build.zig
# =============================================================================

ZIG_PROJECTS=(
    # Small - simple libraries
    "https://github.com/karlseguin/log.zig|small|zig|logging"
    "https://github.com/karlseguin/cache.zig|small|zig|caching"
    "https://github.com/vrischmann/zig-string|small|zig|string library"
    "https://github.com/ziglibs/known-folders|small|zig|XDG paths"
    "https://github.com/nektro/zig-time|small|zig|time library"
    "https://github.com/nektro/zig-glob|small|zig|glob patterns"
    "https://github.com/nektro/zig-flate|small|zig|compression"
    "https://github.com/nektro/zig-base32|small|zig|base32 encoding"
    "https://github.com/nektro/zig-crc|small|zig|CRC checksums"
    "https://github.com/nektro/zig-ryu|small|zig|float formatting"
    "https://github.com/nektro/zig-hash|small|zig|hashing"
    "https://github.com/zigimg/zigimg|small|zig|image library"

    # Medium - application libraries
    "https://github.com/karlseguin/http.zig|medium|zig|HTTP server"
    "https://github.com/karlseguin/pg.zig|medium|zig|PostgreSQL"
    "https://github.com/karlseguin/websocket.zig|medium|zig|WebSocket"
    "https://github.com/vrischmann/zig-sqlite|medium|zig|SQLite"
    "https://github.com/nektro/zig-network|medium|zig|networking"
    "https://github.com/nektro/zig-tar|medium|zig|tar archives"
    "https://github.com/nektro/zig-json|medium|zig|JSON"
    "https://github.com/nektro/zig-xml|medium|zig|XML"
    "https://github.com/nektro/zig-unicode|medium|zig|Unicode"
    "https://github.com/tiehuis/zig-regex|medium|zig|regex"
    "https://github.com/marler182/zig-lz4|medium|zig|LZ4 compression"
    "https://github.com/andrewrk/zig-lib-tracy|medium|zig|Tracy profiler"

    # Large - major applications
    "https://github.com/ghostty-org/ghostty|large|zig|terminal"
    "https://github.com/natecraddock/zf|large|zig|fuzzy finder"
    "https://github.com/zigtools/zls|large|zig|language server"
    "https://github.com/oven-sh/bun|large|zig|JS runtime"
    "https://github.com/tigerbeetle/tigerbeetle|large|zig|database"
    "https://github.com/hexops/mach|large|zig|game engine"
    "https://github.com/Vexu/arocc|large|zig|C compiler"
    "https://github.com/kristoff-it/bork|large|zig|shell"
    "https://github.com/riverwm/river|large|zig|Wayland compositor"
    "https://github.com/ziglang/zig|large|zig|Zig compiler stdlib"
    "https://github.com/andrewrk/tetris|large|zig|game"
)

# =============================================================================
# GO PROJECTS (30) - excellent success rate
# =============================================================================

GO_PROJECTS=(
    # Small (<2k LOC)
    "https://github.com/go-chi/chi|small|go|HTTP router"
    "https://github.com/spf13/cobra|small|go|CLI framework"
    "https://github.com/spf13/viper|small|go|config"
    "https://github.com/sirupsen/logrus|small|go|logging"
    "https://github.com/go-yaml/yaml|small|go|YAML parser"
    "https://github.com/json-iterator/go|small|go|fast JSON"
    "https://github.com/gorilla/mux|small|go|HTTP router"
    "https://github.com/gorilla/websocket|small|go|WebSocket"
    "https://github.com/stretchr/testify|small|go|testing"
    "https://github.com/fatih/color|small|go|terminal colors"

    # Medium (2k-20k LOC)
    "https://github.com/gin-gonic/gin|medium|go|web framework"
    "https://github.com/labstack/echo|medium|go|web framework"
    "https://github.com/gofiber/fiber|medium|go|web framework"
    "https://github.com/go-kit/kit|medium|go|microservices"
    "https://github.com/uber-go/zap|medium|go|logging"
    "https://github.com/rs/zerolog|medium|go|logging"
    "https://github.com/go-playground/validator|medium|go|validation"
    "https://github.com/golang/protobuf|medium|go|protobuf"
    "https://github.com/go-sql-driver/mysql|medium|go|MySQL driver"
    "https://github.com/lib/pq|medium|go|PostgreSQL driver"

    # Large (20k-100k LOC)
    "https://github.com/etcd-io/bbolt|large|go|key-value store"
    "https://github.com/hashicorp/raft|large|go|consensus"
    "https://github.com/dgraph-io/badger|large|go|key-value"
    "https://github.com/syndtr/goleveldb|large|go|LevelDB port"
    "https://github.com/go-git/go-git|large|go|git impl"
    "https://github.com/pion/webrtc|large|go|WebRTC"
    "https://github.com/quic-go/quic-go|large|go|QUIC protocol"
    "https://github.com/gocolly/colly|large|go|scraper"
    "https://github.com/rqlite/rqlite|large|go|distributed SQLite"
    "https://github.com/restic/restic|large|go|backup tool"
)

# =============================================================================
# RUST PROJECTS (35) - cargo builds reliably
# =============================================================================

RUST_PROJECTS=(
    # Small - utilities and small libraries
    "https://github.com/BurntSushi/ripgrep|small|cargo|grep tool"
    "https://github.com/sharkdp/fd|small|cargo|find replacement"
    "https://github.com/sharkdp/bat|small|cargo|cat replacement"
    "https://github.com/ogham/exa|small|cargo|ls replacement"
    "https://github.com/bootandy/dust|small|cargo|du replacement"
    "https://github.com/XAMPPRocky/tokei|small|cargo|code counter"
    "https://github.com/casey/just|small|cargo|task runner"
    "https://github.com/dbrgn/tealdeer|small|cargo|tldr client"
    "https://github.com/ducaale/xh|small|cargo|HTTP client"
    "https://github.com/dalance/procs|small|cargo|ps replacement"
    "https://github.com/Canop/broot|small|cargo|file navigator"
    "https://github.com/lotabout/skim|small|cargo|fuzzy finder"

    # Medium - libraries and tools
    "https://github.com/serde-rs/serde|medium|cargo|serialization"
    "https://github.com/serde-rs/json|medium|cargo|JSON"
    "https://github.com/tokio-rs/tokio|medium|cargo|async runtime"
    "https://github.com/rayon-rs/rayon|medium|cargo|parallelism"
    "https://github.com/clap-rs/clap|medium|cargo|CLI parsing"
    "https://github.com/rust-lang/regex|medium|cargo|regex"
    "https://github.com/hyperium/hyper|medium|cargo|HTTP"
    "https://github.com/actix/actix-web|medium|cargo|web framework"
    "https://github.com/crossbeam-rs/crossbeam|medium|cargo|concurrency"
    "https://github.com/dtolnay/anyhow|medium|cargo|error handling"
    "https://github.com/dtolnay/thiserror|medium|cargo|error derive"
    "https://github.com/rust-lang/log|medium|cargo|logging"

    # Large - major applications
    "https://github.com/starship/starship|large|cargo|prompt"
    "https://github.com/alacritty/alacritty|large|cargo|terminal"
    "https://github.com/nushell/nushell|large|cargo|shell"
    "https://github.com/helix-editor/helix|large|cargo|editor"
    "https://github.com/dandavison/delta|large|cargo|diff viewer"
    "https://github.com/ajeetdsouza/zoxide|large|cargo|cd replacement"
    "https://github.com/meilisearch/meilisearch|large|cargo|search engine"
    "https://github.com/rust-lang/rustfmt|large|cargo|formatter"
    "https://github.com/rust-lang/rust-clippy|large|cargo|linter"
    "https://github.com/nickel-lang/nickel|large|cargo|config language"
    "https://github.com/diem/diem|large|cargo|blockchain"
)
