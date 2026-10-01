-- Keep selected PDF code lines at full color and fade the remaining syntax colors.
-- Example: ```{.r highlight-lines="1,3-4"}
function CodeBlock(block)
  local lines = block.attributes["highlight-lines"]
  if not lines or not quarto.doc.is_format("pdf") then
    return nil
  end
  local selected = {}
  local line_count = select(2, block.text:gsub("\n", "")) + 1
  for item in (lines .. ","):gmatch("(.-),") do
    local first, last = item:match("^%s*(%d+)%s*%-%s*(%d+)%s*$")
    if not first then
      first = item:match("^%s*(%d+)%s*$")
      last = first
    end
    first, last = tonumber(first), tonumber(last)
    if not first or first < 1 or last < first or last > line_count then
      error("highlight-lines must contain valid line numbers or ranges, e.g. 1,3-4")
    end
    for number = first, last do
      selected[number] = true
    end
  end
  local setup = { "\\begingroup" }
  for number = 1, line_count do
    if selected[number] then
      setup[#setup + 1] = "\\expandafter\\def\\csname CodeEmphasisLine:" ..
        number .. "\\endcsname{}"
    end
  end
  setup[#setup + 1] = [[\renewcommand{\FancyVerbFormatText}[1]{%
\ifcsname CodeEmphasisLine:\arabic{FancyVerbLine}\endcsname%
#1%
\else%
\fadecodeline{#1}%
\fi}]]
  block.attributes["highlight-lines"] = nil
  return {
    pandoc.RawBlock("latex", table.concat(setup, "\n")),
    block,
    pandoc.RawBlock("latex", "\\endgroup")
  }
end
