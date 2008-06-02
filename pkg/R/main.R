source('list.R')
source('parse.R')

srcfile <- srcfile('example.R')
srcrefs <- attributes(parse(srcfile$filename,
                            srcfile=srcfile))$srcref
parse.refs(zip.list(prerefs(srcfile), srcrefs))
