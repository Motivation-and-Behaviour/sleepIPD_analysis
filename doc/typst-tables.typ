// Let tables longer than a page break across pages instead of running off the page.
#show figure.where(kind: "quarto-float-tbl"): set block(breakable: true)

// No justified text inside table cells
#show table.cell: set par(justify: false)
