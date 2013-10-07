import ROOT

ROOT.RooMsgService.instance().setGlobalKillBelow(ROOT.RooFit.WARNING)
ROOT.gStyle.SetOptStat(1111111)

# we have to keep these in scope so they don't get garbage collected
_ROODATAHISTS_KEEP = []

ws = ROOT.RooWorkspace()


def iter_collection(rooAbsCollection):
    ''' Create a generator over a RooAbsCollection

    >>> import ROOT
    >>> a = ROOT.RooRealVar("a", "a", 1.0, 0, 2)
    >>> b = ROOT.RooRealVar("b", "b", 2.0, 0, 2)
    >>> argset = ROOT.RooArgSet(a, b)
    >>> [ x.getVal() for x in iter_collection(argset) ]
    [1.0, 2.0]
    '''

    iterator = rooAbsCollection.createIterator()
    object = iterator.Next()
    while object:
        yield object
        object = iterator.Next()


def ws_import(x):
    """ Work around python reserved word """
    getattr(ws, "import")(x)


def import_pdf_from_hist(ws, name, tfile, path_to_hist, roovar, scale, error):
    """ Build a RooHistPDF from a TH1F in a TFile and import into WS """
    th1f = tfile.Get(path_to_hist)
    expevents = th1f.Integral()
    th1f.Scale(scale)
    datahist = ROOT.RooDataHist(
        name + "_hist", th1f.GetTitle(), ROOT.RooArgList(roovar), th1f)
    histpdf = ROOT.RooHistPdf(
        name + "_histpdf", th1f.GetTitle(), ROOT.RooArgSet(roovar),
        datahist)
    _ROODATAHISTS_KEEP.append((th1f, datahist, histpdf))
    the_min_range = 0
    if "sig" in name:
        the_min_range = -100
    # Make normalization variable
    ws.factory("%s_norm[%f, %f, 20000]" % (name, th1f.Integral(),
                                           the_min_range))
    extpdf = ROOT.RooExtendPdf(
        name, th1f.GetTitle(), histpdf, ws.var(name + "_norm")
    )
    getattr(ws, "import")(extpdf)
    # Build the nuisance.  We always constrain to the un-biased events
    ws.factory("RooGaussian::%s_norm_pdf(%s_norm, %s_mean[%f], %s_sigma[%f])" %
               (name, name, name, expevents,
                name, expevents * error))

mass = ws.factory("mass[20, 300]")

# Get raw pdfs
f3_mmt_file = ROOT.TFile("mmt_f3_shapes.root", "READ")
sig_mmt_file = ROOT.TFile("llt_sig_shapes.root", "READ")

# Import them into the workspace
import_pdf_from_hist(
    ws, "f3_wz", f3_mmt_file, "mmtCatLow/wz", mass, 1, 0.06)
import_pdf_from_hist(
    ws, "f3_zz", f3_mmt_file, "mmtCatLow/zz", mass, 1, 0.1)
import_pdf_from_hist(
    ws, "f3_fakes", f3_mmt_file, "mmtCatLow/fakes", mass, 1, 0.5)
import_pdf_from_hist(
    ws, "f3_fakes_biased", f3_mmt_file, "mmtCatLow/fakes", mass, 0.7, 0.5)

import_pdf_from_hist(
    ws, "sig_wz", sig_mmt_file, "mmtCatLow/wz", mass, 1, 0.06)
import_pdf_from_hist(
    ws, "sig_zz", sig_mmt_file, "mmtCatLow/zz", mass, 1, 0.1)
import_pdf_from_hist(
    ws, "sig_fakes", sig_mmt_file, "mmtCatLow/fakes", mass, 1, 0.1)
import_pdf_from_hist(
    ws, "sig_fakes_biased", sig_mmt_file, "mmtCatLow/fakes", mass, 0.7, 0.1)
import_pdf_from_hist(
    ws, "sig_signal", sig_mmt_file, "mmtCatLow/WH125", mass, 1, 0.1)

# Make the composite PDFs
f3_expected = ROOT.RooAddPdf(
    "f3_expected", "Expected events in F3 region",
    ROOT.RooArgList(ws.pdf("f3_wz"), ws.pdf("f3_zz"), ws.pdf("f3_fakes")))

f3_expected_biased = ROOT.RooAddPdf(
    "f3_expected_biased", "Expected events in F3 region, biased",
    ROOT.RooArgList(ws.pdf("f3_wz"), ws.pdf("f3_zz"),
                    ws.pdf("f3_fakes_biased")))

sig_expected = ROOT.RooAddPdf(
    "sig_expected", "Expected events in F3 region",
    ROOT.RooArgList(ws.pdf("sig_wz"), ws.pdf("sig_zz"), ws.pdf("sig_fakes"),
                    ws.pdf("sig_signal")))

sig_expected_biased = ROOT.RooAddPdf(
    "sig_expected_biased", "Expected events in F3 region, biased",
    ROOT.RooArgList(ws.pdf("sig_wz"), ws.pdf("sig_zz"),
                    ws.pdf("sig_fakes_biased"), ws.pdf("sig_signal")))

ws_import(f3_expected)
ws_import(f3_expected_biased)
ws_import(sig_expected)
ws_import(sig_expected_biased)

print "Expected events in F3: %s" % (
    f3_expected.expectedEvents(ROOT.RooArgSet(mass)))
print "Expected events in biased F3: %s" % (
    f3_expected_biased.expectedEvents(ROOT.RooArgSet(mass)))

# Save snapshots
ws.saveSnapshot("snap", ws.allVars())

# Expected signal yield - our POI
expected_signal = ws.var("sig_signal_norm").getVal()
print "Expected signal: %0.4f" % expected_signal


def run_toys(f3_generate_pdf, f3_fit_pdf,
             sig_generate_pdf, sig_fit_pdf,
             ntoys, fake_error_strategy, img_file):
    canvas = ROOT.TCanvas("asdf", "asdf", 800, 800)
    output = ROOT.TH1F("signal_pull", "signal_pull", 100, -5, 5)
    for i in range(ntoys):
        ws.loadSnapshot("snap")
        print "Fitting toy %i" % i
        f3_pseudo_data = f3_generate_pdf.generateBinned(
            ROOT.RooArgSet(mass), ROOT.RooFit.Extended())
        f3_fit_result = f3_fit_pdf.fitTo(
            f3_pseudo_data,
            ROOT.RooFit.Save(True),
            ROOT.RooFit.ExternalConstraints(ROOT.RooArgSet(
                ws.pdf("f3_wz_norm_pdf"), ws.pdf("f3_zz_norm_pdf"),
                ws.pdf("f3_fakes_norm_pdf")
            )),
            ROOT.RooFit.PrintLevel(-1)
        )
        # print "f3: %i" % f3_fit_result.status()

        # apply our fake error strategy - move f3 post fit errors into
        # sig constraints, etc.
        fake_error_strategy(f3_fit_result)

        # Now fit the signal region
        sig_pseudo_data = sig_generate_pdf.generate(
            ROOT.RooArgSet(mass), ROOT.RooFit.Extended())

        sig_fit_pdf.fitTo(
            sig_pseudo_data,
            ROOT.RooFit.Save(True),
            ROOT.RooFit.ExternalConstraints(ROOT.RooArgSet(
                ws.pdf("sig_wz_norm_pdf"), ws.pdf("sig_zz_norm_pdf"),
                ws.pdf("sig_fakes_norm_pdf")
            )),
            ROOT.RooFit.PrintLevel(-1)
        )
        # sig_fit_result.Print("v")
        # print "sig: %i" % sig_fit_result.status()
        # for ipar in range(f3_npars):
        #     prefit = f3_fit_result.floatParsInit()[ipar]
        #     postfit = f3_fit_result.floatParsFinal()[ipar]
        #     print "%20s prefit:  %0.3f +- %0.3f" % (
        #         prefit.GetName(), prefit.getVal(), prefit.getError())
        #     print "%20s postfit: %0.3f +- %0.3f" % (
        #         postfit.GetName(), postfit.getVal(), postfit.getError())
        sig_residual = ws.var("sig_signal_norm").getVal() - expected_signal
        sig_pull = sig_residual / ws.var("sig_signal_norm").getError()

        output.Fill(sig_pull)
    output.Draw()
    canvas.SaveAs(img_file)


def do_nothing_strategy(f3_fit_result):
    pass


def current_strategy(f3_fit_result):
    # Set to max of F3 pull and postfit
    f3_fakes_mean_prefit = f3_fit_result.floatParsInit().find(
        "f3_fakes_norm")
    f3_fakes_mean_postfit = f3_fit_result.floatParsFinal().find(
        "f3_fakes_norm")
    delta = abs(f3_fakes_mean_postfit.getVal() - f3_fakes_mean_prefit.getVal())
    delta /= f3_fakes_mean_prefit.getVal()
    f3_sigma = f3_fakes_mean_postfit.getError() / f3_fakes_mean_postfit.getVal()
    ws.var("sig_fakes_sigma").setVal(
        max(f3_sigma, delta) * ws.var("sig_fakes_norm").getVal())


def new_strategy(f3_fit_result):
    # Set to max of F3 pull and postfit
    f3_fakes_mean_prefit = f3_fit_result.floatParsInit().find(
        "f3_fakes_norm")
    f3_fakes_mean_postfit = f3_fit_result.floatParsFinal().find(
        "f3_fakes_norm")
    delta = abs(f3_fakes_mean_postfit.getVal() - f3_fakes_mean_prefit.getVal())
    delta /= f3_fakes_mean_prefit.getVal()
    scale = f3_fakes_mean_postfit.getVal() / f3_fakes_mean_prefit.getVal()
    sig_mean_current = ws.var("sig_fakes_mean").getVal()
    print "Scaling by: %f" % scale
    ws.var("sig_fakes_mean").setVal(sig_mean_current * scale)
    f3_sigma = f3_fakes_mean_postfit.getError() / f3_fakes_mean_postfit.getVal()
    ws.var("sig_fakes_sigma").setVal(
        max(f3_sigma, delta) * ws.var("sig_fakes_norm").getVal())


ntoys = 1000

pulls = run_toys(f3_expected, f3_expected,
                 sig_expected, sig_expected,
                 ntoys, do_nothing_strategy, "do_nothing.png")
pulls = run_toys(f3_expected, f3_expected,
                 sig_expected, sig_expected,
                 ntoys, current_strategy, "current.png")
pulls = run_toys(f3_expected, f3_expected,
                 sig_expected, sig_expected,
                 ntoys, new_strategy, "new.png")
pulls = run_toys(f3_expected_biased, f3_expected,
                 sig_expected_biased, sig_expected,
                 ntoys, do_nothing_strategy, "do_nothing_biased.png")
pulls = run_toys(f3_expected_biased, f3_expected,
                 sig_expected_biased, sig_expected,
                 ntoys, current_strategy, "current_biased.png")
pulls = run_toys(f3_expected_biased, f3_expected,
                 sig_expected_biased, sig_expected,
                 ntoys, new_strategy, "new_biased.png")
