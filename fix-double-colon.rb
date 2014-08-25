require 'asciidoctor'
require 'asciidoctor/extensions'

include ::Asciidoctor

class FixDoubleColon < Extensions::Preprocessor
  def process document, reader
      Reader.new reader.readlines.map { |line|
          (line.include? "`") ? line.gsub("::", "{two-colons}") : line
      }
  end
end

Extensions.register :fixdoublecolon do |document|
    document.preprocessor FixDoubleColon
end
