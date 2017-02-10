# Slightly enhanced plyr functions
#
# Author: Renaud Gaujoux
# Created: Jul 13, 2014
###############################################################################

#' @importFrom pkgmaker sVariable ns_get
#' @importFrom stats setNames
#' @import plyr
NULL

`%||%` <- function(a, b) if(is.null(a)) b else a

#' Apply Functions over Named Objects
#' 
#' Slightly modified versions of \pkg{plyr}'s functions, which give access
#' to the current item's name within the applied function.
#' 
#' @param .data Data object passed to the corresponding \pkg{plyr} function 
#' @param .fun Function to apply to each -- margin -- element of \code{.data}
#' @param ... extra arguments passed to corresponding \pkg{plyr} function
#' 
#' @name nplyr
NULL

.apply_idx <- sVariable(0L)
.apply_name_current <- sVariable()
.apply_set_idx <- function(i){
    oi <- .apply_idx(i)
    cn <- .apply_names()[i] %||% i
    .apply_name_current(cn)
    oi
}
#' @export
#' @rdname nplyr
.iname <- function(){
    .apply_name_current()
}
#' @export
#' @rdname nplyr
.inum <- function(){
    .apply_idx()
}

.apply_names <- sVariable()
.apply_set_names <- function(x, margin = NULL, reset = NULL){
    
    idx <- 0L
    if( !is.null(reset) ){
        idx <- reset$idx
        nam <- reset$names
    }else if( is.null(x) ) nam <- NULL
    else{
        if( is.null(margin) ){
            nam <- names(x)
            n <- length(x)
        }else{
            nam <- .apply_names(dimnames(x)[[margin]])
            n <- dim(x)[margin]   
        }
        if( is.null(nam) ) nam <- as.character(seq(n))
    }
    
    on <- .apply_names(nam)
    oi <- .apply_set_idx(idx)
    list(names = on, idx = oi)
} 
#' @export 
#' @rdname nplyr
.inames <- function(){
    .apply_names()
}
#' @export 
#' @rdname nplyr
.ilength <- function(){
    length(.apply_names())
}


# Wrapper function generator for plyr functions
.ply_wrap <- function(.data, .fun, .margins = NULL){    
    .previous <- .apply_set_names(.data, margin = .margins)
    function(.data, ...){
        .apply_set_idx(.inum() + 1L)
        # restore old values
        on.exit({
            if( .inum() == .ilength() ) .apply_set_names(reset = .previous)    
        })
        
        .fun(.data, ...)
    }
}

.lply_wrap <- function(pfun){
    function(.data, .fun, ...){
        if( is.null(names(.data)) && is.character(.data) ) .data <- setNames(.data, .data)
        pfun <- match.fun(pfun)
        .funw <- .ply_wrap(.data, .fun)
        pfun(.data, .funw, ...)
    }
} 

#' @export 
#' @rdname nplyr
nl_ply <- .lply_wrap('l_ply')

#' @export
#' @rdname nplyr
nldply <- .lply_wrap('ldply')

#' @export
#' @rdname nplyr
nllply <- .lply_wrap('llply')

#' @param .margins Margin over which to apply 
#' 
#' @export 
#' @rdname nplyr
na_ply <- function(.data, .margins, .fun, ...){
    o <- .inames()
    on.exit( .apply_names(o) )
    .funw <- .ply_wrap(.data, .fun, .margins)
    a_ply(.data, .margins, .funw, ...)
}

#' @export
#' @rdname nplyr
nadply <- function(.data, .margins, .fun, ...){
    o <- .inames()
    on.exit( .apply_names(o) )
    .funw <- .ply_wrap(.data, .fun, .margins)
    adply(.data, .margins, .funw, ...)
}

#' @export
#' @rdname nplyr
nalply <- function(.data, .margins, .fun, ...){
    o <- .inames()
    on.exit( .apply_names(o) )
    .funw <- .ply_wrap(.data, .fun, .margins)
    setNames(alply(.data, .margins, .funw, ...), dimnames(.data)[[.margins]])
}

is_knitting <- function(){
    isNamespaceLoaded('knitr') && !is.null(ns_get('knit_concord', 'knitr')$get('inlines'))
}

.ifig_tag <- function(i = if(.ilength()>1L) .inum(), ext = NULL){
    if( is_knitting() ){
        if( is.null(ext) ) ext <- opts_current$get('fig.ext')
        if( is.null(ext) ) ext <- 'png'
        img <- paste0(fig_path(number = i), '.', ext)
        sprintf("<img src=\"%s\" /><div class=\"caption\"></div>\n", img)
    }
}

# Apply function along list-like object, wrapping output in Bootstrap group of collapse.  
bootstrap_collapse <- function(id, .data, .fun, ..., title = NULL, style = '', applyr = 'l_ply'){
    
    # ensure one uses use named version of plyr function
    if( is.character(applyr) && !grepl("^n", applyr) )
        applyr <- paste0('n', applyr)
    applyr <- match.fun(applyr)
    
    # simple apply if not knitting a chunk
    if( !is_knitting() ) return( applyr(.data, .fun, ...) )
    
    # bootstrap wrapper
    .bfun <- function(...){
        
        # first item: output group opening <div>
        if( .inum() == 1L ) cat(sprintf('<div class="panel-group" id="%s" style="%s">\n', id, style))
        
        # auto generate title if needed
        if( is.null(title) ) title <- .iname()
        else title <- title[.inum()]
        if( !length(title) ) title <- paste0('Section ', .inum())
        
        # generate unique panel ID
        item_id <- tolower(paste0(id, '_', .inum(), "_", gsub("[ .]", "_", substr(.iname(), 1, 10))))
        
        # output panel header <div> and body opening <div>
        cat(sprintf('<div class="panel panel-default">
  <div class="panel-heading">
    <h4 class="panel-title">
      <a data-toggle="collapse" data-parent="#%s" href="#collapse_%s">%s</a>
    </h4>
  </div>
  <div id="collapse_%s" class="panel-collapse collapse in">
    <div class="panel-body">\n', id, item_id, title, item_id))
        
        # setup item closing
        on.exit({
            cat('</div></div></div>\n')
            
            # if last item output group closing <div> + script to enable
            if( .inum() == .ilength() ){
                cat('</div>\n') # close group
                # enable
                cat(sprintf("<script>$('.collapse').collapse()</script>\n", id))
            } 
        })
        .fun(...)
    }
    
    applyr(.data, .bfun, ...)
}


bootstrap_tabs <- function(id, .data, .fun, ..., title = NULL, style = '', applyr = 'l_ply', fade = FALSE){
    
    # ensure one uses use named version of plyr function
    if( is.character(applyr) && !grepl("^n", applyr) )
        applyr <- paste0('n', applyr)
    applyr <- match.fun(applyr)
    
    # simple apply if not knitting a chunk
    if( !is_knitting() ) return( applyr(.data, .fun, ...) )
    
    .item_ids <- ''
    # bootstrap wrapper
    .bfun <- function(...){
        
        # first item: output tab links
        item_class <- if( fade ) 'fade' else ''
        if( .inum() == 1L ){
            
            # auto generate tab title if needed
            if( is.null(title) ) title <- .inames()
            if( !length(title) ) title <- paste0('Tab ', seq(.ilength()))
            
            # generate unique tab IDs
            .item_ids <<- tolower(paste0(id, '_', seq_along(title), "_", gsub("[ .]", "_", substr(title, 1, 10))))
            
            item_class <- paste0(item_class, ' in active')        
            cat(sprintf('<!-- Nav tabs -->\n<ul id="%s" class="nav nav-tabs" role="tablist">\n', id))
            sapply(seq_along(title), function(i){
                # first tab is active by default
                class <- if(i == 1L) 'class="active"' else ''
                cat(sprintf('  <li %s><a href="#%s" role="tab" data-toggle="tab">%s</a></li>\n', class, .item_ids[i], title[i]))
            })
            cat(sprintf('</ul>\n<!-- Tab panes -->\n<div class="tab-content" style="%s">\n', style))
        }
        
        # open item div
        cat(sprintf('<div class="tab-pane %s" id="%s">\n', item_class, .item_ids[.inum()]))                            

        # setup item closing
        on.exit({
                cat('</div>\n')
                
                # if last item output group closing <div> + script to enable
                if( .inum() == .ilength() ){
                    cat('</div>\n') # close group
                    # enable
                    cat(sprintf("<script>
$('#%s a').click(function (e) {
  e.preventDefault()
  $(this).tab('show')
})</script>\n", id))
                } 
            })
        .fun(...)
    }
    
    applyr(.data, .bfun, ...)
}


bootstrap_carousel <- function(id, .data, .fun, ..., title = NULL, style = '', applyr = 'l_ply', wrap = FALSE, interval = FALSE, as.knit = FALSE){
    
    # ensure one uses use named version of plyr function
    if( is.character(applyr) && !grepl("^n", applyr) )
        applyr <- paste0('n', applyr)
    applyr <- match.fun(applyr)
    
    # simple apply if not knitting a chunk
    if( !as.knit && !is_knitting() ) return( applyr(.data, .fun, ...) )
    
    id <- gsub("[ .]", "_", id)
    # bootstrap wrapper
    .bfun <- function(...){
        
        # auto generate slide caption if needed
        if( is.null(title) ) title <- .iname()
        else title <- title[.inum()]
        if( !length(title) ) title <- paste0('Slide ', .inum())
        # first item: output indicators
        item_class <- ''
        if( .inum() == 1L ){
            item_class <- ' active'
            if( isTRUE(interval) ) interval <- 5000 
            opt <- sprintf('data-wrap="%s" data-interval="%s"', tolower(wrap), tolower(interval))
            cat(sprintf('<!-- Carousel -->\n<div id="%s" class="carousel slide" %s data-ride="carousel">\n  <!-- Indicators -->\n  <ol class="carousel-indicators">\n', id, opt))
            sapply(seq(.ilength()), function(i){
                # first tab is active by default
                class <- if(i == 1L) 'class="active"' else ''
                cat(sprintf('    <li data-target="#%s" data-slide-to="%s" %s></li>\n', id, i-1L, class))
            })
            cat('  </ol>\n  <!-- Wrapper for slides -->\n  <div class="carousel-inner">\n')
        }
        
        # open item div
        cat(sprintf('    <div class="item%s">\n', item_class))                            
        
        # setup item closing
        on.exit({
                cat(sprintf('      <div class="carousel-caption">\n%s\n      </div>\n    </div>\n', title))
                
                # if last item output group closing <div> + script to enable
                if( .inum() == .ilength() ){
                    cat('  </div>\n') # close inner
                    # controls
                    cat(sprintf('  <!-- Controls -->
  <a class="left carousel-control" href="#%s" role="button" data-slide="prev">
    <span class="glyphicon glyphicon-chevron-left">&lsaquo;</span>
  </a>
  <a class="right carousel-control" href="#%s" role="button" data-slide="next">
    <span class="glyphicon glyphicon-chevron-right">&rsaquo;</span>
  </a>
</div>\n', id, id))
                    # enable
                    cat(sprintf("<script>$('#%s').carousel()</script>\n", id))
                } 
        })
        .fun(...)
    }
    
    applyr(.data, .bfun, ...)
}
