using Metal

MAX_THREADS_PER_GROUP = 1024

n = 8192
N = 128

A = rand(Float32, n, n)
x = rand(Float32, n)

function matvec_cpu!(y::Vector{Float32}, A::Matrix{Float32}, x::Vector{Float32})
    y .= A * x
end

function matvec_cpu_loop!(y::Vector{Float32}, A::Matrix{Float32}, x::Vector{Float32})
    m = size(A, 1)
    n = size(A, 2)
    y .= 0.0
    @inbounds for j in 1:n  # Column major indexing needs j outside for cache locality
        @inbounds for i in 1:m
            y[i] += A[i, j] * x[j]
        end
    end
end

function matvec_gpu!(y::MtlArray{Float32}, A::MtlArray{Float32}, x::MtlArray{Float32})
    y .= A * x
end

function matvec_kernel(A, x, y, m, n)
    i = thread_position_in_grid_1d()
    if i <= m
        s = 0.0f0
        for j in 1:n
            s += A[i, j] * x[j]
        end
        y[i] = s
    end
    return
end

function matvec_gpu_loop!(y::MtlArray{Float32}, A::MtlArray{Float32}, x::MtlArray{Float32})
    m = size(A, 1)
    n = size(A, 2)
    threads_per_group = min(m, MAX_THREADS_PER_GROUP)
    num_groups = cld(m, threads_per_group)
    @metal threads=threads_per_group groups=num_groups matvec_kernel(A, x, y, m, n)
end

function bench_cpu()
    println("Benchmarking CPU implementations...")

    A_cpu = copy(A)
    x_cpu = copy(x)
    y_cpu = zeros(Float32, n)

    matvec_cpu!(y_cpu, A_cpu, x_cpu)
    @time for _ in 1:N
        matvec_cpu!(y_cpu, A_cpu, x_cpu)
    end

    matvec_cpu_loop!(y_cpu, A_cpu, x_cpu)
    @time for _ in 1:N
        matvec_cpu_loop!(y_cpu, A_cpu, x_cpu)
    end
end

function bench_gpu()
    println("\nBenchmarking GPU implementation...")

    A_gpu = MtlArray(A)
    x_gpu = MtlArray(x)
    y_gpu = MtlArray(zeros(Float32, n))

    matvec_gpu!(y_gpu, A_gpu, x_gpu)
    @time for _ in 1:N
        matvec_gpu!(y_gpu, A_gpu, x_gpu)
    end

    matvec_gpu_loop!(y_gpu, A_gpu, x_gpu)
    @time for _ in 1:N
        matvec_gpu_loop!(y_gpu, A_gpu, x_gpu)
    end
end

bench_cpu()
bench_gpu()
