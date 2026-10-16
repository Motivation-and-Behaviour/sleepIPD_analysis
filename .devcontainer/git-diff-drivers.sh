#!/bin/sh
# Text conversions for the diff drivers named in .gitattributes, so `git diff`
# and `git log -p` show rendered .docx and .pdf files as text.
git config diff.docx.textconv "pandoc --from=docx --to=markdown --wrap=none"
git config diff.docx.cachetextconv true
git config diff.pdf.textconv "sh -c 'pdftotext -layout \"\$0\" -'"
git config diff.pdf.cachetextconv true
