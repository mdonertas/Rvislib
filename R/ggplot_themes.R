#' Scales for ggplot2 shapes
#'
#' shapes_rvis maps discrete variables to shapes. The major difference between ggplot2 scale_shape function and shapes_rvis is the number of shapes. With this function you can map up to 17 shapes. If you have more than 17 levels, they will not be displayed - if you have more than 17 levels, it is probably not the best approach to use shapes for mapping.
#'
#' @param ... common discrete scale parameters: name, breaks, labels, na.value, limits and guide. See discrete_scale for more details
#'
#' @examples
#' # create a dataframe with 17 distinct values
#' d=data.frame(a=letters[1:17])
#' library(ggplot2)
#' # plot shapes
#' ggplot() +
#'   scale_x_discrete(name="") +
#'   scale_y_continuous(limits=c(0,1)) +
#'   geom_point(data=d, mapping=aes(x=a, y=0.5, shape=a), size=5)+
#'   shapes_rvis()
#'
#' @seealso \code{\link{scale_shape}}, \code{\link{discrete_scale}}
#' @importFrom ggplot2 discrete_scale
#' @importFrom scales manual_pal
#' @export
#'
shapes_rvis <- function(...){
  ggplot2::discrete_scale("shape","rvis",
                          scales::manual_pal(unname(c(16,15,17,18,1,0,2,5,4,7:14))),...)
}

#' Custom ggplot2 theme
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param x.text.angle rotation angle of x axis tick labels. Use 90 for vertical orientation. Defaults to 0.
#' @param spacing space between facets (in cm). Defaults to 0.1.
#' @param legend.pos character specifying the position of legend. 'top','bottom','left','right', or a vector of two. Defaults to 'top'.
#' @param legend.dir direction of the keys in a legend.
#' @param leg.box.order direction in which multiple legends are displayed
#' @param just justification parameter as accepted by legend.box.just
#' @param just.par justification parameter as accepted by legend.justification
#'
#' @examples
#' library(ggplot2)
#' # Plot with the default theme
#' ggplot(mpg, aes(x = class, fill = drv)) + geom_bar() +
#'   ggtitle('Plot with the default theme')
#' # Plot with theme_rvis
#' ggplot(mpg, aes(x = class, fill = drv)) + geom_bar() +
#'   theme_rvis(x.text.angle = 60, legend.pos = "top")+
#'   ggtitle('Using custom theme function')
#'
#' @import ggplot2
#' @seealso \code{\link{theme}}
#' @export
#'
theme_rvis <- function(base_size = 8, base_family = "Helvetica", x.text.angle=0,
                       spacing = 0.1, legend.pos='top',
                       legend.dir = NA, leg.box.order = NA,
                       just = NA, just.par = NA) {
  requireNamespace('ggplot2')
  half_line = base_size / 2
  if(any(legend.pos == 'left') | any(legend.pos == 'right')){
    legend.dirx = 'vertical'
    leg.box.orderx = 'vertical'
    just.parx = 'center'
    justx = 'left'
  } else if(any(legend.pos == 'top') | any(legend.pos == 'bottom')) {
    legend.dirx = 'horizontal'
    leg.box.orderx = 'horizontal'
    just.parx = 'center'
    justx = 'top'
  } else {
    legend.dirx = 'vertical'
    leg.box.orderx = 'vertical'
    just.parx = c(0,0)
    justx = 'left'
  }
  legend.dir = ifelse(is.na(legend.dir),legend.dirx,legend.dir)
  leg.box.order = ifelse(is.na(leg.box.order),leg.box.orderx,leg.box.order)
  just = ifelse(is.na(just),justx,just)
  just.par = ifelse(is.na(just.par),just.parx,just.par)

  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    theme(axis.text = element_text(size = rel(1), colour = 'grey25'),
          axis.text.x = element_text(margin = margin(t = 0.1 * half_line/2),
                                     vjust = ifelse(x.text.angle %in% c(0,90), 0.5 , 1),
                                     hjust = ifelse(x.text.angle==0, 0.5, 1),
                                     angle=x.text.angle),
          axis.text.x.top = element_text(margin = margin(b = 0.1 *  half_line/2),
                                         vjust = 0),
          axis.text.y = element_text(margin = margin(r = 0.1 * half_line/2),
                                     hjust = 1),
          axis.text.y.right = element_text(margin = margin(l = 0.1 * half_line/2),
                                           hjust = 0),
          axis.ticks = element_line(colour = 'gray25'),
          axis.title = element_text(size = rel(1.2)),
          axis.title.x = element_text(margin = margin(t = half_line/2), vjust = 1),
          axis.title.x.top = element_text(margin = margin(b = half_line/2), vjust = 0),
          axis.title.y = element_text(angle = 90, margin = margin(r = half_line/2), vjust = 1),
          axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line/2), vjust = 0),
          panel.border = element_rect(fill=NA, color = 'gray25'),
          panel.grid.major = element_line(color = 'gray90', size = 0.15),
          panel.grid.minor = element_line(color = 'gray90', size = 0.05),
          panel.spacing = unit(spacing,'cm'),
          strip.background = element_rect(fill = 'grey25', colour = 'grey25'),
          strip.text = element_text(colour = "white", size = rel(1.2), hjust=0.5),
          strip.text.x = element_text(margin = margin(t = half_line/3, b = half_line/3)),
          strip.text.y = element_text(angle = -90, margin = margin(l = half_line/3, r = half_line/3)),
          strip.placement = 'outside',
          plot.title = element_text(size = rel(1.5), hjust = 0, vjust = 1, margin = margin(b = half_line * 0.5)),
          plot.subtitle = element_text(size = rel(1.2), hjust = 0, vjust = 1, margin = margin(b = half_line * 0.5), face = 'italic'),
          plot.caption = element_text(size = rel(1), hjust = 1, vjust = 1, margin = margin(t = half_line * 0.5), face = 'italic'),
          plot.margin = margin(half_line/2,half_line/2,half_line/2,half_line/2,'pt'),
          legend.background = element_rect(fill = ifelse(legend.pos %in% c('top','bottom','right','left'), NA, 'gray90'),
                                           color = ifelse(legend.pos %in% c('top','bottom','right','left'), NA, 'gray25')),
          legend.key = element_rect(fill = ifelse(legend.pos %in% c('top','bottom','right','left'), NA, 'gray90')),
          legend.spacing = unit(half_line/2,'pt'),
          legend.margin = margin(1,2,1,2,'pt'),
          legend.text = element_text(size = rel(1)),
          legend.box.just = just,
          legend.justification = just.par,
          legend.title.align = 0.5,
          legend.key.height = unit(base_size*0.8,'pt'),
          legend.key.width = unit(base_size*0.8,'pt'),
          legend.position = legend.pos,
          legend.direction = legend.dir,
          legend.box = leg.box.order,
          legend.title = element_text(face= 'italic', size = rel(1.1)),
          legend.box.background = element_rect(fill=NA,
                                               color = ifelse(!legend.pos %in% c('top','bottom','right','left'), NA, 'gray25'),
                                               size = ifelse(!legend.pos %in% c('top','bottom','right','left'), 0, 0.2)),
          legend.box.margin = margin(1,1,1,1,'pt'),
          legend.box.spacing = unit(half_line/2,'pt'))
}
