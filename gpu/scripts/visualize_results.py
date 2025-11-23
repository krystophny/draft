#!/usr/bin/env python3

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import sys
from pathlib import Path

def load_results(csv_file):
    return pd.read_csv(csv_file)

def plot_performance_comparison(df, output_dir):
    kernels = df['Kernel'].unique()

    fig, axes = plt.subplots(2, 2, figsize=(16, 12))
    fig.suptitle('GPU Benchmark Performance Comparison', fontsize=16, fontweight='bold')

    for idx, kernel in enumerate(kernels):
        if idx >= 3:
            break
        ax = axes.flatten()[idx]
        kernel_data = df[df['Kernel'] == kernel]

        implementations = kernel_data['Implementation'].unique()
        x = np.arange(len(implementations))
        width = 0.35

        cpu_times = []
        gpu_times = []
        labels = []

        for impl in implementations:
            impl_data = kernel_data[kernel_data['Implementation'] == impl]
            cpu_data = impl_data[impl_data['Device'] == 'CPU']
            gpu_data = impl_data[impl_data['Device'] == 'GPU']

            cpu_time = cpu_data['Time_ms'].values[0] if len(cpu_data) > 0 else 0
            gpu_time = gpu_data['Time_ms'].values[0] if len(gpu_data) > 0 else 0

            cpu_times.append(cpu_time)
            gpu_times.append(gpu_time)
            labels.append(impl)

        bars1 = ax.bar(x - width/2, cpu_times, width, label='CPU', alpha=0.8)
        bars2 = ax.bar(x + width/2, gpu_times, width, label='GPU', alpha=0.8)

        ax.set_ylabel('Time (ms)', fontweight='bold')
        ax.set_title(f'{kernel} Performance', fontweight='bold')
        ax.set_xticks(x)
        ax.set_xticklabels(labels, rotation=45, ha='right')
        ax.legend()
        ax.grid(True, alpha=0.3)

        for bar in bars1:
            height = bar.get_height()
            if height > 0:
                ax.text(bar.get_x() + bar.get_width()/2., height,
                       f'{height:.2f}',
                       ha='center', va='bottom', fontsize=8)

        for bar in bars2:
            height = bar.get_height()
            if height > 0:
                ax.text(bar.get_x() + bar.get_width()/2., height,
                       f'{height:.2f}',
                       ha='center', va='bottom', fontsize=8)

    speedup_ax = axes.flatten()[3]
    for kernel in kernels:
        kernel_data = df[df['Kernel'] == kernel]
        implementations = kernel_data['Implementation'].unique()

        speedups = []
        impl_labels = []

        for impl in implementations:
            impl_data = kernel_data[kernel_data['Implementation'] == impl]
            cpu_data = impl_data[impl_data['Device'] == 'CPU']
            gpu_data = impl_data[impl_data['Device'] == 'GPU']

            if len(cpu_data) > 0 and len(gpu_data) > 0:
                cpu_time = cpu_data['Time_ms'].values[0]
                gpu_time = gpu_data['Time_ms'].values[0]
                if gpu_time > 0:
                    speedup = cpu_time / gpu_time
                    speedups.append(speedup)
                    impl_labels.append(f'{impl}\n({kernel})')

        x_pos = np.arange(len(impl_labels))
        speedup_ax.bar(x_pos, speedups, alpha=0.7, label=kernel)

    speedup_ax.set_ylabel('Speedup (CPU/GPU)', fontweight='bold')
    speedup_ax.set_title('GPU Speedup vs CPU', fontweight='bold')
    speedup_ax.set_xticks(x_pos)
    speedup_ax.set_xticklabels(impl_labels, rotation=45, ha='right', fontsize=8)
    speedup_ax.axhline(y=1, color='r', linestyle='--', alpha=0.5, label='1x (no speedup)')
    speedup_ax.legend()
    speedup_ax.grid(True, alpha=0.3)

    plt.tight_layout()
    output_file = output_dir / 'performance_comparison.png'
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f'Saved performance comparison to {output_file}')
    plt.close()

def plot_gflops_comparison(df, output_dir):
    fig, ax = plt.subplots(figsize=(14, 8))

    kernels = df['Kernel'].unique()
    implementations = df['Implementation'].unique()

    x = np.arange(len(kernels))
    width = 0.15

    for idx, impl in enumerate(implementations):
        gpu_gflops = []
        for kernel in kernels:
            data = df[(df['Kernel'] == kernel) &
                     (df['Implementation'] == impl) &
                     (df['Device'] == 'GPU')]
            gflops = data['GFLOPS'].values[0] if len(data) > 0 else 0
            gpu_gflops.append(gflops)

        offset = (idx - len(implementations)/2) * width
        bars = ax.bar(x + offset, gpu_gflops, width, label=impl, alpha=0.8)

        for bar in bars:
            height = bar.get_height()
            if height > 0:
                ax.text(bar.get_x() + bar.get_width()/2., height,
                       f'{height:.1f}',
                       ha='center', va='bottom', fontsize=8, rotation=0)

    ax.set_ylabel('GFLOPS', fontweight='bold')
    ax.set_title('GPU Compute Performance (GFLOPS)', fontweight='bold', fontsize=14)
    ax.set_xticks(x)
    ax.set_xticklabels(kernels)
    ax.legend()
    ax.grid(True, alpha=0.3, axis='y')

    plt.tight_layout()
    output_file = output_dir / 'gflops_comparison.png'
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f'Saved GFLOPS comparison to {output_file}')
    plt.close()

def plot_bandwidth_comparison(df, output_dir):
    fig, ax = plt.subplots(figsize=(14, 8))

    kernels = df['Kernel'].unique()
    implementations = df['Implementation'].unique()

    x = np.arange(len(kernels))
    width = 0.15

    for idx, impl in enumerate(implementations):
        gpu_bandwidth = []
        for kernel in kernels:
            data = df[(df['Kernel'] == kernel) &
                     (df['Implementation'] == impl) &
                     (df['Device'] == 'GPU')]
            bw = data['Bandwidth_GBs'].values[0] if len(data) > 0 else 0
            gpu_bandwidth.append(bw)

        offset = (idx - len(implementations)/2) * width
        bars = ax.bar(x + offset, gpu_bandwidth, width, label=impl, alpha=0.8)

        for bar in bars:
            height = bar.get_height()
            if height > 0:
                ax.text(bar.get_x() + bar.get_width()/2., height,
                       f'{height:.1f}',
                       ha='center', va='bottom', fontsize=8, rotation=0)

    ax.set_ylabel('Bandwidth (GB/s)', fontweight='bold')
    ax.set_title('GPU Memory Bandwidth', fontweight='bold', fontsize=14)
    ax.set_xticks(x)
    ax.set_xticklabels(kernels)
    ax.legend()
    ax.grid(True, alpha=0.3, axis='y')

    plt.tight_layout()
    output_file = output_dir / 'bandwidth_comparison.png'
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f'Saved bandwidth comparison to {output_file}')
    plt.close()

def generate_report(df, output_dir):
    report_file = output_dir / 'benchmark_report.txt'

    with open(report_file, 'w') as f:
        f.write('='*80 + '\n')
        f.write('GPU BENCHMARK REPORT\n')
        f.write('Plasma Physics Kernels Performance Analysis\n')
        f.write('='*80 + '\n\n')

        f.write('SUMMARY STATISTICS\n')
        f.write('-'*80 + '\n\n')

        for kernel in df['Kernel'].unique():
            f.write(f'\n{kernel} Kernel:\n')
            kernel_data = df[df['Kernel'] == kernel]

            for impl in kernel_data['Implementation'].unique():
                impl_data = kernel_data[kernel_data['Implementation'] == impl]

                cpu_data = impl_data[impl_data['Device'] == 'CPU']
                gpu_data = impl_data[impl_data['Device'] == 'GPU']

                if len(cpu_data) > 0 and len(gpu_data) > 0:
                    cpu_time = cpu_data['Time_ms'].values[0]
                    gpu_time = gpu_data['Time_ms'].values[0]
                    speedup = cpu_time / gpu_time if gpu_time > 0 else 0

                    f.write(f'  {impl}:\n')
                    f.write(f'    CPU Time:    {cpu_time:10.3f} ms\n')
                    f.write(f'    GPU Time:    {gpu_time:10.3f} ms\n')
                    f.write(f'    Speedup:     {speedup:10.2f}x\n')
                    f.write(f'    GPU GFLOPS:  {gpu_data["GFLOPS"].values[0]:10.2f}\n')
                    f.write(f'    GPU BW:      {gpu_data["Bandwidth_GBs"].values[0]:10.2f} GB/s\n')

        f.write('\n' + '='*80 + '\n')
        f.write('\nRECOMMENDATIONS\n')
        f.write('-'*80 + '\n\n')

        gpu_data = df[df['Device'] == 'GPU']
        for kernel in gpu_data['Kernel'].unique():
            kernel_data = gpu_data[gpu_data['Kernel'] == kernel]
            fastest = kernel_data.loc[kernel_data['Time_ms'].idxmin()]
            f.write(f'{kernel}: Fastest implementation is {fastest["Implementation"]} ')
            f.write(f'({fastest["Time_ms"]:.3f} ms, {fastest["GFLOPS"]:.2f} GFLOPS)\n')

        f.write('\n' + '='*80 + '\n')

    print(f'Generated report: {report_file}')

    with open(report_file, 'r') as f:
        print('\n' + f.read())

def main():
    results_dir = Path('results')
    csv_file = results_dir / 'benchmark_results.csv'

    if not csv_file.exists():
        print(f'Error: {csv_file} not found. Run benchmarks first.')
        sys.exit(1)

    df = load_results(csv_file)

    print('Generating visualizations...')
    plot_performance_comparison(df, results_dir)
    plot_gflops_comparison(df, results_dir)
    plot_bandwidth_comparison(df, results_dir)

    print('\nGenerating report...')
    generate_report(df, results_dir)

    print('\nVisualization complete!')

if __name__ == '__main__':
    main()
