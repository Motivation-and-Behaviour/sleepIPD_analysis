-- tinytable writes raw Typst, which citeproc never sees. Replace each [@key]
-- in it with that entry's formatted reference.

local function keys(cite)
  local ids = {}
  for id in cite:gmatch("@([^;%s%]]+)") do
    ids[#ids + 1] = id
  end
  return ids
end

function Pandoc(doc)
  local ids = {}
  doc:walk({
    RawBlock = function(el)
      if el.format == "typst" then
        for cite in el.text:gmatch("%[@[^%]]-%]") do
          for _, id in ipairs(keys(cite)) do
            ids[#ids + 1] = id
          end
        end
      end
    end,
  })
  if #ids == 0 then
    return nil
  end

  -- Format just these entries with the document's bibliography and CSL
  local refs = pandoc.Pandoc({}, doc.meta)
  refs.meta.nocite = pandoc.read("[@" .. table.concat(ids, "; @") .. "]", "markdown").blocks[1].content
  local entries = {}
  pandoc.utils.citeproc(refs):walk({
    Div = function(div)
      local id = div.identifier:match("^ref%-(.+)$")
      if id then
        -- Drop the Lancet entry number (csl-left-margin)
        div:walk({
          Span = function(span)
            if span.classes:includes("csl-right-inline") then
              entries[id] = pandoc.write(pandoc.Pandoc({ pandoc.Plain(span.content) }), "typst"):gsub("%s+$", "")
            end
          end,
        })
      end
    end,
  })

  return doc:walk({
    RawBlock = function(el)
      if el.format ~= "typst" then
        return nil
      end
      el.text = el.text:gsub("%[@[^%]]-%]", function(cite)
        local out = {}
        for _, id in ipairs(keys(cite)) do
          if not entries[id] then
            pandoc.log.warn("typst-citations: " .. id .. " not found in bibliography")
          end
          out[#out + 1] = entries[id] or ("#strong[" .. id .. "?]")
        end
        return table.concat(out, " \\ ")
      end)
      return el
    end,
  })
end
