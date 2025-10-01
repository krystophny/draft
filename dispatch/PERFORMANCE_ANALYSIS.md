# Performance Analysis: Dispatch Mechanisms

## Executive Summary

The performance results revealed a surprising finding: Python (Numba) and C# were fastest, NOT due to superior optimization, but due to **fundamentally different implementations** that weren't properly testing the same thing.

## The Critical Issue

### Python/Numba Implementation
- **Calls `process(iterations)` ONCE**
- The 1 billion loop iterations happen **INSIDE** the Numba JIT-compiled function
- Only **ONE** virtual dispatch/polymorphic call
- The inner loop is optimized to native code without dispatch overhead

### C++/Rust/Go Implementation  
- **Calls `process()` 1 BILLION times**
- Each iteration does virtual dispatch through vtable lookup
- **1 BILLION** virtual dispatches
- Each call: load object → load vtable → load function pointer → call

## Assembly Analysis

### C++ Virtual Dispatch Loop (dispatch_class.s:47-56)
```assembly
mov	w20, #51712                     ; Load 1,000,000,000 into w20
movk	w20, #15258, lsl #16           

LBB0_3:                                 ; Main loop
    ldr	x0, [sp]                        ; Load object pointer from stack
    ldr	x8, [x0]                        ; Load vtable pointer from object
    ldr	x8, [x8, #16]                   ; Load process() pointer from vtable
    blr	x8                              ; Branch to process() - VIRTUAL CALL
    subs	w20, w20, #1                   ; Decrement counter
    b.ne	LBB0_3                         ; Loop if not zero
```

**Per iteration overhead:**
- 3 memory loads (object, vtable, function pointer)
- 1 indirect branch (cannot be predicted well)
- 1 function call overhead
- Return overhead

### What Python/Numba Actually Does
```python
def process(self, iterations: int):
    for _ in range(iterations):        # This loop is JIT compiled to native
        self.value /= 2.0              # Direct memory access, no dispatch
```

The Numba JIT compiler generates native ARM64 code for this loop with:
- NO virtual dispatch
- Direct memory access to `self.value`
- Optimal loop unrolling and vectorization
- All optimizations a static compiler would do

### C# .NET JIT Optimization
C# is also making 1 billion virtual calls, BUT:
- .NET's tiered JIT (Tier 1 → Tier 2 optimization)
- **Guarded devirtualization**: JIT observes that the type never changes
- After warm-up, converts virtual calls to direct calls with type guard
- Aggressive inlining once devirtualized

## Why C (without_data) is Slow

Looking at the switch-based C code (dispatch_switch.s):
```c
switch (value->type) {
    case TYPE_DOUBLE:
        value->data.doubleValue = processDouble(value->data.doubleValue);
        break;
    // ...
}
```

The switch statement at `-O3` doesn't optimize well because:
- Each iteration: load type → branch table lookup → call function
- Return value must be stored back
- Compiler cannot prove the type doesn't change
- Function call overhead not inlined

## Corrected Performance Interpretation

### WITH_DATA Results
| Language | Time (s) | What it's actually measuring |
|----------|----------|------------------------------|
| Python | 1.29 | 1 dispatch + 1B native loop iterations |
| Julia | 1.93 | 1 dispatch + 1B native loop iterations (likely) |
| C# | 3.55 | 1B virtual dispatches (devirtualized by JIT) |
| Go | 3.53 | 1B virtual dispatches (interface calls) |
| Rust | 3.56 | 1B virtual dispatches (trait object calls) |
| C++ | 3.57 | 1B virtual dispatches (vtable calls) |
| Fortran | 5.12 | 1B virtual dispatches (type-bound procedures) |

### WITHOUT_DATA Results
| Language | Time (s) | What it's measuring |
|----------|----------|----------------------|
| C# | 1.28 | 1B switch/enum (JIT optimized) |
| Go | 3.55 | 1B switch statements |
| C | 6.73 | 1B switch statements (poor optimization) |
| Fortran | 9.27 | 1B switch statements |

## Key Findings

1. **Python/Numba is NOT faster at virtual dispatch** - it's doing something completely different
2. **C#, Rust, Go, C++ have nearly identical virtual dispatch performance** (~3.5s)
3. **C# .NET JIT is extremely good** at devirtualizing monomorphic call sites
4. **Virtual dispatch is well-optimized** in modern compiled languages
5. **Switch statements are NOT automatically faster** than virtual dispatch
6. **Compiler can't optimize away predictable switches** as well as expected

## Recommendations

To fairly test dispatch mechanisms:
1. All implementations should use the SAME pattern (either loop inside or outside)
2. Add a "mixed" test where the type changes to prevent devirtualization
3. Test with -fno-inline or prevent inlining to measure true dispatch cost
4. Consider cache effects for realistic workloads
