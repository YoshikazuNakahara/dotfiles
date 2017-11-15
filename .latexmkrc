$latex = 'uplatex %O -kanji=utf8 -no-guess-input-enc -synctex=1 -interaction=nonstopmode %S';↲
$pdflatex = 'lualatex %O -synctex=1 -interaction=nonstopmode %S';↲
$bibtex = 'upbibtex %O %B';↲
$biber = 'biber %O --bblencoding=utf8 -u -U --output_safechars %B';↲
$makeindex = 'upmendex %O -o %D %S';↲
$dvipdf = 'dvipdfmx %O -o %D %S';↲
$dvips = 'dvips %O -z -f %S | convbkmk -u > %D';↲
$ps2pdf = 'ps2pdf %O %S %D';↲
$pdf_mode = 3;↲
$max_repeat = 10;↲
$pdf_previewer = 'zathura -reuse-instance %O %S';↲
