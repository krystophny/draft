CODE = getenv('CODE');
addpath([CODE '/external/MARS/RZplot']);

global Mac
global SDIR

SDIR = ['RUN/MARS/'];

Mac.mm_plot = [];
Mac.edge = 1.0;
Mac.core = 0.0;
Mac.plot_VM = 0;
Mac.plot_VS = 0;
Mac.Norm = 1.0;

MacConvBoozerCustom

disp(['Mac.Norm = ' num2str(Mac.Norm)]);
