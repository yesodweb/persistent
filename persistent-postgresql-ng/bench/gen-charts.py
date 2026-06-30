#!/usr/bin/env python3
"""Generate benchmark comparison SVGs for the README."""

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np

# Color scheme
COLOR_OLD = '#6c757d'   # gray for persistent-postgresql
COLOR_NEW = '#0d6efd'   # blue for persistent-postgresql-ng

def make_chart(title, benchmarks, old_times, new_times, filename, unit='ms'):
    fig, ax = plt.subplots(figsize=(12, 6))

    x = np.arange(len(benchmarks))
    width = 0.35

    bars_old = ax.bar(x - width/2, old_times, width, label='persistent-postgresql', color=COLOR_OLD)
    bars_new = ax.bar(x + width/2, new_times, width, label='persistent-postgresql-ng', color=COLOR_NEW)

    ax.set_ylabel(f'Time ({unit})', fontsize=12)
    ax.set_title(title, fontsize=14, fontweight='bold')
    ax.set_xticks(x)
    ax.set_xticklabels(benchmarks, rotation=25, ha='right', fontsize=10)
    ax.legend(fontsize=11)
    ax.grid(axis='y', alpha=0.3)

    # Add speedup labels on the new bars
    for i, (old, new) in enumerate(zip(old_times, new_times)):
        if old > 0 and new > 0:
            speedup = old / new
            if speedup >= 1.3:
                ax.annotate(f'{speedup:.0f}x',
                    xy=(x[i] + width/2, new),
                    xytext=(0, 5), textcoords='offset points',
                    ha='center', fontsize=9, fontweight='bold', color='#0a58ca')

    fig.tight_layout()
    fig.savefig(filename, format='svg', bbox_inches='tight')
    plt.close(fig)
    print(f'Wrote {filename}')


# --- 0ms latency ---
benchmarks_0ms = [
    'get x100',
    'insert x100',
    'upsert x100',
    'delete x100',
    'update x100',
    'insertMany x1000',
    'selectList x100',
    'mixed DML x100',
]
old_0ms = [4.7, 12.8, 12.7, 12.9, 12.5, 14.1, 11.2, 29.9]
new_0ms = [1.7, 10.8,  8.9,  9.6,  9.4,  5.3,  8.6, 14.6]

make_chart(
    'Benchmark: 0ms latency (localhost)',
    benchmarks_0ms, old_0ms, new_0ms,
    'persistent-postgresql-ng/bench/bench-0ms.svg'
)

# --- 1ms latency ---
benchmarks_1ms = [
    'get x100',
    'insert x100',
    'upsert x100',
    'delete x100',
    'update x100',
    'replace x100',
    'insertMany x1000',
    'selectList x100',
    'deleteWhere x100',
]
old_1ms = [310, 314, 321, 592, 555, 602, 31, 25.8, 750]
new_1ms = [ 11,  13,  13,  25,  25,  27, 8.6, 16.6, 119]

make_chart(
    'Benchmark: 1ms latency per direction (2ms RTT)',
    benchmarks_1ms, old_1ms, new_1ms,
    'persistent-postgresql-ng/bench/bench-1ms.svg'
)

# --- 5ms latency ---
benchmarks_5ms = [
    'get x100',
    'insert x100',
    'insertMany x1000',
    'selectList x100',
    'select IN x20',
]
old_5ms = [1190, 1200, 72.6, 74.0, 70.3]
new_5ms = [  50,   41, 22.8, 47.9, 44.1]

make_chart(
    'Benchmark: 5ms latency per direction (10ms RTT)',
    benchmarks_5ms, old_5ms, new_5ms,
    'persistent-postgresql-ng/bench/bench-5ms.svg'
)
