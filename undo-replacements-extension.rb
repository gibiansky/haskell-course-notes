require 'asciidoctor'
require 'asciidoctor/extensions'

include ::Asciidoctor

class UndoReplacements < Extensions::Postprocessor
  Replacements = [/&#8594;/, /&#8658;/]
  Originals = ["->", "=>"]

  def process document, output
    Replacements.zip(Originals).each do |replacement, original|
        output = output.gsub(replacement) { original }
    end
    output
  end
end

Extensions.register :undo do |document|
    document.postprocessor UndoReplacements
end
