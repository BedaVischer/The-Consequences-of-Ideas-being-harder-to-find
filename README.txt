================================================================================
README - DATA AVAILABILITY AND REPLICATION PACKAGE
The Consequences of Ideas Being Harder to Find: Evidence from US Patents
Beda Carl Vischer, London School of Economics (b.c.vischer@lse.ac.uk)
================================================================================

This file specifies the data required to replicate the empirical results in the paper. Data are organized by source. For each source I list the
provider, access conditions, the variables drawn from it, and the role it plays
in the analysis.


--------------------------------------------------------------------------------
1. COMPUSTAT (FIRM-LEVEL ACCOUNTS)
--------------------------------------------------------------------------------
Provider     : S&P Global Market Intelligence, via Wharton Research Data
               Services (WRDS).
Access       : Proprietary. Requires an institutional WRDS subscription. Not
               redistributable. Users must obtain their own license.
Files        : Compustat North America Fundamentals Annual.
Variables    : sales (SALE), cost of goods sold (COGS), capital (PPEGT, PPENT),
               employment (EMP), SG&A (XSGA), R&D expenditure (XRD), operating
               income, NAICS and SIC industry codes, GVKEY, fiscal year.
Coverage     : US publicly listed firms, 1950-present. Estimation sample
               restricted to the period described in the paper.
Role         : Construction of firm-level markups (translog, via the
               De Ridder-Grassi-Morzenti production function methodology), TFP,
               and R&D intensity. Matched to patent data via GVKEY.


--------------------------------------------------------------------------------
2. PATEX EXAMINER DATA (INSTRUMENT)
--------------------------------------------------------------------------------
Provider     : USPTO Patent Examination Research Dataset (PatEx), Office of the
               Chief Economist.
Access       : Public. Downloadable at no cost from the USPTO website.
Files        : Application data, transaction history, examiner assignment files.
Variables    : examiner identifier, art unit, application filing and disposal
               dates, grant/abandonment outcomes, application serial numbers.
Role         : Construction of the quasi-random examiner leniency instrument
               (leave-one-out grant rate within art unit by year) used in the
               LP-IV design. Examiner assignment is conditionally as-good-as-
               random within art unit and cohort.


--------------------------------------------------------------------------------
3. PATENT GRANT AND CITATION DATA
--------------------------------------------------------------------------------
Provider     : USPTO PatentsView and the USPTO bulk data repository.
Access       : Public. Downloadable at no cost.
Files        : Granted patent records, assignee disambiguation files, citation
               files, CPC/USPC classification files.
Variables    : patent number, grant date, application date, assignee identifier,
               assignee name, technology class, forward and backward citations.
Role         : Linking patents to firms, classifying patents, and constructing
               citation-based measures. Provides the patent-side identifiers
               used in the firm-patent match.


--------------------------------------------------------------------------------
4. KPST PATENT VALUE / IMPORTANCE SCORES
--------------------------------------------------------------------------------
Provider     : Kogan, Papanikolaou, Seru, and Stoffman (2017), market-value-
               based patent importance estimates.
Access       : Public. Posted in the authors' online replication archive and on
               Noah Stoffman's website.
Files        : Patent-level value estimates (nominal and real, citation-adjusted
               variants).
Variables    : patent number, estimated dollar value, filing/grant year.
Role         : Weighting patents by economic importance and distinguishing
               high-value from low-value patents in the productive-versus-
               defensive classification.


--------------------------------------------------------------------------------
5. CENSUS BUSINESS DYNAMICS STATISTICS (BDS)
--------------------------------------------------------------------------------
Provider     : US Census Bureau, Business Dynamics Statistics.
Access       : Public. Downloadable at no cost from the Census website. Uses
               only published tabulations, not restricted microdata.
Files        : BDS tables by sector and by firm age/size.
Variables    : job creation and destruction rates, establishment entry and exit
               rates, firm entry rates, startup share of employment.
Role         : Business dynamism outcomes in the reduced-form and LP-IV
               analysis. Measures the decline in dynamism the paper links to
               defensive R&D.


--------------------------------------------------------------------------------
6. INDUSTRY CROSSWALKS
--------------------------------------------------------------------------------
Provider     : US Census Bureau (NAICS-SIC concordances) and USPTO/NBER
               technology-to-industry concordances.
Access       : Public.
Files        : SIC-NAICS crosswalk tables; patent-class-to-industry concordance.
Role         : Harmonizing industry codes across Compustat (SIC and NAICS), BDS
               (NAICS), and patent technology classes so that firm, patent, and
               industry data align on a common industry definition.



================================================================================
NOTES ON ACCESS AND CONFIDENTIALITY
================================================================================
- Compustat is the only proprietary input and cannot be redistributed. All
  code that uses it is included; the data must be obtained independently via
  WRDS.
- All USPTO, Census BDS, KPST, and crosswalk inputs are public and can be
  downloaded at no cost; download scripts and retrieval dates are provided.
- No restricted-access Census microdata are used; all dynamism measures come
  from published BDS tabulations.

================================================================================
