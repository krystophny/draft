CODE = getenv('CODE');
DATA = getenv('DATA');
addpath([CODE '/external/MARS/RZplot']);

global Mac
global SDIR

SDIR = [DATA '/DEMO/MARS/MARSQ_OUTPUTS_100kAt_dBkinetic_NTVkinetic_NEO2profs_KEYTORQ_1/'];

Mac.mm_plot = [];
Mac.edge = 1.0;
Mac.core = 0.0;
Mac.plot_VM = 0;
Mac.plot_VS = 0;
Mac.Norm = 1.0;

MacConvBoozerCustom

disp(['Mac.Norm = ' num2str(Mac.Norm)]);
