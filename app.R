library(shiny)
library(shinyjs)
library(highcharter)
library(billboarder)
library(waypointer)
library(tidyverse)

# creates 100vh div
longdiv <- function(...){
    div(
        ...,
        class = "container",
        style = "height:100vh;"
    )
}

OFFSET <- '50%'
ANIMATION <- 'slideInUp'

ui <- fluidPage(
    tags$head(
        tags$link(rel = 'stylesheet', href = 'style.css'),
        tags$link(
            rel = 'stylesheet',
            href = 'https://use.fontawesome.com/releases/v5.8.1/css/all.css',
            integrity = 'sha384-50oBUHEmvpQ+1lW4y57PTFmhCaXp0ML5d60M1M7uH2+nqUivzIebhndOJK28anvf',
            crossorigin = 'anonymous'
        )
    ),
    use_waypointer(),
    div(
        id = 'bg',
        div(
            id = 'stick',
            style = 'position:fixed;width:100%;',
            fluidRow(
                column(4),
                column(8, uiOutput('right_ui'))
            )
        ),
        longdiv(
            h1('debaseR', class = 'title'),
            br(),
            br(),
            h1(
                class = 'subtitle',
                'Did the Pixies really invent dynamic shifts in rock music?'
            ),
            br(),
            p(
                style = 'text-align:center;',
                'Data pulled from the Spotify Web API via spotifyr', 
                tags$a(
                    class = 'sg',
                    tags$i(class = 'fas fa-external-link-alt'),
                    target = '_blank',
                    href = 'https://www.rcharlie.com/spotifyr/'
                )
            ),
            br(),
            br(),
            br(),
            p(
                style = 'text-align:center;',
                tags$i(class = 'fas fa-chevron-down fa-3x')
            )
        ),
        map(1:5, function(x) {
            longdiv(
                div(
                    id = str_glue('m{x}'),
                    uiOutput(str_glue('test_{x}'))
                )
            )
        })
    )
)

server <- function(input, output, session) {
    
    last_active <- reactiveVal(0)
    
    for (i in 1:5) {
        assign(str_glue('w{i}'), {
            Waypoint$
                new(str_glue('m{i}'), offset = OFFSET, animate = TRUE, animation = ANIMATION)$
                start()
        })
    }
    
    pane_1 <- tagList(
        h1('Background'),
        h3('The Pixies have often been cited for using strong', strong(' dynamic shifts '), 'in their music, wherein they shift drastically between quiet and loud within their songs. 
       Kurt Cobain cited it as a huge influence on Nirvana\'s "Smells Like Teen Spirit."'),
        img(src = 'https://images.kerrangcdn.com/Kurt_Header.jpeg?auto=compress&fit=crop&w=', width = '50%'),
        tags$blockquote(
            '"I was basically trying to rip off the Pixies...We used their sense of dynamics, being soft and quiet and then loud and hard."'
        ) 
    )
    
    pane_2 <- tagList(
        h3('Further examples abound: A 2006 documentary about the band was titled ', em('loudQuietLoud,'), 
           'and Gary Smith, the producer of their debut album, ', em('Come on Pilgrim'), ', even went so far as to say that they started a trend within pop music:'),
        tags$img(src = 'http://www.gstatic.com/tv/thumb/v22vodart/164934/p164934_v_v8_aa.jpg', width = '25%'),
        tags$blockquote('"sooner or later, all sorts of bands were exploiting the same strategy of wide dynamics. It became a kind of new pop formula"')
    )
    
    pane_3 <- tagList(
        h3('The Pixies\' frontman, Black Francis, admitted in 1991 that their style was intentional.'),
        img(src = 'https://static.stereogum.com/uploads/2018/03/pixies-1521647446-640x417.jpg'),
        tags$blockquote(
            '"We do try to be dynamic, but it\'s dumbo dynamics, because we don\'t know how to do anything else. We can play loud or quiet—that\'s it."'
        )
    )
    
    pane_4 <- tagList(
        h3("This analysis explores the dynamics of the Pixies' music and whether they really deserve credit for being loud and quiet.")
    )
    
    pane_5 <- tagList(
        h3('Using data from Spotify, we can plot the loudness of each song over time.')
    )
    
    map(1:5, function(x) {
        output[[str_glue('test_{x}')]] <- renderUI({
            req(get(str_glue('w{x}'))$get_triggered())
            if (get(str_glue('w{x}'))$get_triggered() == TRUE) {
                get(str_glue('pane_{x}'))
            }
        })
    })
    
    right_pane_1 <- tagList(
        ''
    )
    
    right_pane_2 <- tagList(
        ''
    )
    
    right_pane_3 <- tagList(
        ''
    )
    
    right_pane_4 <- tagList(
        highchartOutput('graph', width = '100%', height = '100vh')
    )
    
    right_pane_5 <- tagList(
        ''
    )
    
    output$right_ui <- renderUI({
        last_active()
        if (is.null(w2$get_direction())) {
            pane <- ''
        } else {
            pane <- get(str_glue('right_pane_{last_active()}'))
        }
        return(pane)
    })
    
    output$graph <- renderHighchart({
        species <- c('setosa', 'versicolor', 'virginica')
        iris %>% 
            filter(Species %in% species[1:last_active()]) %>% 
            hchart(hcaes(x = Sepal.Length, y = Sepal.Width, group = Species), type = 'scatter')
    })
    
    map(1:5, function(x) {
        observeEvent(get(str_glue('w{x}'))$get_direction(), {
            if (get(str_glue('w{x}'))$get_direction() %in% c('down', 'up')) {
                last_active(x)
            }
        })
    })
    
}

shinyApp(ui, server)