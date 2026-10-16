-- Quarto puts each captioned docx table inside a one-cell layout table; lift the caption and table out
local function flatten(blocks)
  local out = pandoc.Blocks({})
  for _, b in ipairs(blocks) do
    if b.t == "Div" then
      out:extend(flatten(b.content))
    else
      out:insert(b)
    end
  end
  return out
end

function Table(tbl)
  local rows = tbl.bodies[1] and tbl.bodies[1].body or {}
  if #tbl.bodies ~= 1 or #rows ~= 1 or #rows[1].cells ~= 1 or #tbl.head.rows > 0 then
    return nil
  end
  local content = flatten(rows[1].cells[1].contents)
  for _, b in ipairs(content) do
    if b.t == "Table" then
      return content
    end
  end
  return nil
end
