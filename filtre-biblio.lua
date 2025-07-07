-- filtre-biblio.lua

local List = require 'pandoc.List'
local utils = require 'pandoc.utils'

-- s'assurer que même les références non mentionnées sont présentes
function RawBlock(el)
  if el.format == "markdown" and el.text:match("^@%*%s*%[filtre=") then
    local filtre = el.text:match("@%*%s*%[filtre=(.-)%]")
    if filtre then
      local bibitems = {}
      for _, item in pairs(PANDOC_STATE.references or {}) do
        if item.citation and item.citation["filtre"] == filtre then
          table.insert(bibitems, item)
        end
      end
      -- Trier par auteur
      table.sort(bibitems, function(a, b)
        return (a.author or "") < (b.author or "")
      end)

      local bullets = {}
      for _, ref in ipairs(bibitems) do
        table.insert(bullets, pandoc.BulletList({{pandoc.Plain({pandoc.Str(ref.citation)})}}))
      end
      return pandoc.Div(bullets, {class = "refs"})
    end
  end
end

-- prvious code gardé
function Meta(meta)
  
  local bibfiles = meta.bibliography
  if not bibfiles then return end

  if type(bibfiles) == "table" then
    bibliography_files = bibfiles
  else
    bibliography_files = { bibfiles }
  end
end

function Div(el)
  if el.identifier ~= "refs" then return nil end

  -- Chargement bib
  local refs = {}
  for _, bibfile in ipairs(bibliography_files or {}) do
    local entries = utils.citeproc.load_bibliography(bibfile)
    for _, e in ipairs(entries) do
      table.insert(refs, e)
    end
  end

  -- Regroupement par filtres personnalisés
  local groups = {
    metamorphoses = { title = "Références des textes des *Métamorphoses*", entries = {} },
    antiquite     = { title = "Œuvres antiques", entries = {} },
    outils        = { title = "Outils numériques", entries = {} },
    scientifique  = { title = "Articles et ouvrages scientifiques", entries = {} }
  }

  for _, ref in ipairs(refs) do
    local filtre = ref.filtre or "scientifique"
    if groups[filtre] then
      table.insert(groups[filtre].entries, ref)
    end
  end

  -- Tri alphabétique
  for _, group in pairs(groups) do
    table.sort(group.entries, function(a, b)
      return (a.author or a.title or "") < (b.author or b.title or "")
    end)
  end

  -- Construction du contenu
  local blocks = List()
  for _, group in pairs(groups) do
    if #group.entries > 0 then
      table.insert(blocks, pandoc.Header(2, pandoc.read(group.title, "markdown").blocks[1].content))
      local items = List()
      for _, entry in ipairs(group.entries) do
        local cite = pandoc.Cite({pandoc.Str("")}, {{id = entry.id}})
        table.insert(items, {pandoc.Plain{cite}})
      end
      table.insert(blocks, pandoc.BulletList(items))
    end
  end

  return pandoc.Div(blocks, el.attr)
end
