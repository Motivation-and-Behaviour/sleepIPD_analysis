--[[
docx-sections: give every Word section the page set-up of doc/reference.docx.

Quarto's `.landscape` divs end sections with bare <w:sectPr> elements: an empty
one for the portrait pages before the div, and one holding only an A4
landscape <w:pgSz> after it. Word gives a section without a footer reference
no footer if it is the first section, and falls back to its own page size and
margins, so everything before the first landscape table lost its page numbers
and A4 paper.

This fills those gaps with the values in doc/reference.docx: the footer's
relationship id (rIdFtr1), A4 portrait, and 2.54 cm margins. If the reference
document changes, change these to match. Run it at post-render, after Quarto
has inserted the section breaks.
]]

local footer = '<w:footerReference w:type="default" r:id="rIdFtr1" />'
local portrait = '<w:pgSz w:w="11906" w:h="16838" />'
local margins = '<w:pgMar w:top="1440" w:right="1440" w:bottom="1440" '
  .. 'w:left="1440" w:header="720" w:footer="720" w:gutter="0" />'

function RawBlock(el)
  if el.format ~= "openxml" then
    return nil
  end
  local inner = el.text:match("<w:sectPr>(.-)</w:sectPr>")
  if inner == nil then
    return nil
  end
  -- Only rewrite Quarto's bare section breaks: nothing in them but whitespace
  -- and, for the landscape break, a page size
  local page_size = inner:match("<w:pgSz[^>]*/>")
  local rest = inner:gsub("<w:pgSz[^>]*/>", "")
  if rest:match("%S") then
    return nil
  end
  -- CT_SectPr order: footer reference, then page size, then margins
  local section = "<w:sectPr>" .. footer .. (page_size or portrait) .. margins
    .. "</w:sectPr>"
  el.text = el.text:gsub("<w:sectPr>.-</w:sectPr>", function() return section end)
  return el
end
