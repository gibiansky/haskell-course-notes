require 'asciidoctor'
require 'asciidoctor/extensions'

include ::Asciidoctor

class FixPygments < Extensions::Postprocessor
  def process document, output
    output.gsub(/><i class="conum"/, "> <i class=\"conum\"")
  end
end

Extensions.register :fixpygments do |document|
    document.postprocessor FixPygments
end
