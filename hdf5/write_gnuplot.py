import numpy as np
import subprocess

# Create sample data
x = np.linspace(0, 2 * np.pi, 100)  # Array of 100 points from 0 to 2π
y = np.sin(x)  # Compute sin(x)

# Prepare the data as a string
data = '\n'.join(f'{xi} {yi}' for xi, yi in zip(x, y))

# Gnuplot commands
gnuplot_commands = """
set terminal pdfcairo
set output 'plot.pdf'
set title "Plot of y = sin(x)"
set xlabel "x"
set ylabel "y"
plot '-' with lines title 'sin(x)'
"""

# Call Gnuplot and pipe the data
process = subprocess.Popen(['gnuplot'], stdin=subprocess.PIPE, text=True)
process.stdin.write(gnuplot_commands + '\n')  # Send Gnuplot commands
process.stdin.write(data + '\n')  # Send the data
process.stdin.write('e\n')  # End of data marker
process.stdin.flush()  # Ensure all data is sent
process.stdin.close()  # Close the pipe
process.wait()  # Wait for Gnuplot to finish
