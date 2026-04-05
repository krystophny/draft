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
    # Large C projects only (>20k LOC) - for fair comparison
    "https://github.com/lua/lua|large|make|scripting language"
    "https://github.com/rui314/chibicc|large|make|C compiler"
    "https://github.com/lz4/lz4|large|cmake|compression"
    "https://github.com/facebook/zstd|large|cmake|compression"
    "https://github.com/redis/hiredis|large|cmake|Redis client"
    "https://github.com/libevent/libevent|large|cmake|event library"
    "https://github.com/libuv/libuv|large|cmake|async I/O"
    "https://github.com/madler/zlib|large|cmake|compression"
    "https://github.com/glennrp/libpng|large|cmake|PNG handling"
    "https://github.com/curl/curl|large|cmake|HTTP client"
    "https://github.com/openssl/openssl|large|cmake|crypto library"
    "https://github.com/libgit2/libgit2|large|cmake|git library"
    "https://github.com/redis/redis|large|make|database"
    "https://github.com/nginx/nginx|large|make|web server"
    "https://github.com/tmux/tmux|large|cmake|terminal multiplexer"
    "https://github.com/git/git|large|make|version control"
    "https://github.com/vim/vim|large|make|text editor"
    "https://github.com/FFmpeg/FFmpeg|large|make|media framework"
    "https://github.com/micropython/micropython|large|make|Python for MCU"
    "https://github.com/raysan5/raylib|large|cmake|game library"
    "https://github.com/libexpat/libexpat|large|cmake|XML parser"
    "https://github.com/warmcat/libwebsockets|large|cmake|WebSocket library"
    "https://github.com/libjpeg-turbo/libjpeg-turbo|large|cmake|JPEG codec"
    "https://github.com/webmproject/libwebp|large|cmake|WebP codec"
    "https://github.com/libsndfile/libsndfile|large|cmake|audio I/O"
    "https://github.com/libffi/libffi|large|cmake|FFI library"
    "https://github.com/libusb/libusb|large|cmake|USB library"
    "https://github.com/libarchive/libarchive|large|cmake|archive library"
    "https://github.com/jedisct1/libsodium|large|cmake|crypto library"
    "https://github.com/apache/apr|large|cmake|portable runtime"
)

# =============================================================================
# C++ PROJECTS - mix of small and large for fair comparison
# =============================================================================

CPP_PROJECTS=(
    # Small C++ projects (<5k LOC) - compiled libraries, not header-only
    "https://github.com/zeux/pugixml|small|cmake|XML parser"
    "https://github.com/yhirose/cpp-peglib|small|cmake|PEG parser"
    "https://github.com/bombela/backward-cpp|small|cmake|stack traces"
    "https://github.com/adishavit/argh|small|cmake|argument parser"
    "https://github.com/mpark/variant|small|cmake|variant implementation"

    # Medium C++ projects (5k-20k LOC)
    "https://github.com/google/googletest|medium|cmake|testing framework"
    "https://github.com/google/benchmark|medium|cmake|microbenchmarks"
    "https://github.com/ArthurSonzogni/FTXUI|medium|cmake|TUI library"
    "https://github.com/litehtml/litehtml|medium|cmake|HTML renderer"
    "https://github.com/jbeder/yaml-cpp|medium|cmake|YAML parser"
    "https://github.com/catchorg/Catch2|medium|cmake|testing framework"
    "https://github.com/fmtlib/fmt|medium|cmake|formatting library"
    "https://github.com/gabime/spdlog|medium|cmake|logging library"

    # Large C++ projects (>20k LOC) - real compiled code
    "https://github.com/simdjson/simdjson|large|cmake|fast JSON parser"
    "https://github.com/google/re2|large|cmake|regex engine"
    "https://github.com/google/snappy|large|cmake|compression"
    "https://github.com/google/flatbuffers|large|cmake|serialization"
    "https://github.com/google/leveldb|large|cmake|key-value store"
    "https://github.com/abseil/abseil-cpp|large|cmake|base library"
    "https://github.com/bulletphysics/bullet3|large|cmake|physics engine"
    "https://github.com/libjpeg-turbo/libjpeg-turbo|large|cmake|JPEG codec"
    "https://github.com/glfw/glfw|large|cmake|window library"
    "https://github.com/assimp/assimp|large|cmake|3D model importer"
    "https://github.com/facebook/folly|large|cmake|Facebook library"
    "https://github.com/grpc/grpc|large|cmake|RPC framework"
    "https://github.com/protocolbuffers/protobuf|large|cmake|serialization"
    "https://github.com/boostorg/beast|large|cmake|HTTP/WebSocket"
    "https://github.com/opencv/opencv|large|cmake|computer vision"
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
