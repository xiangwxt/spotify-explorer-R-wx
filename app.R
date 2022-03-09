# author: Wenxin Xiang
# date: 2022-03-07

library("dash")
library("dashBootstrapComponents")
library("dashHtmlComponents")
library("dashCoreComponents")
library("ggplot2")
library("tidyverse")
library("tidyr")
library("plotly")


# Read raw data
df <-  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")
df <- drop_na(df)



# Set up app frontend
app <- Dash$new(
    external_stylesheets = dbcThemes$MINTY,
    suppress_callback_exceptions = T,
)
app$title("Spotify Explorer")



# Styling
NAV_STYLE <- list(
    "height" = "50px",
    "fontSize" = "large"
)

FOOTER_STYLE <- list(
    "position" = "fixed",
    "bottom" = 0,
    "left" = 0,
    "right" = 0,
    "height" = "25px",
    "padding" = "3px 0 0 5px",
    "backgroundColor" = "green",
    "color" = "white",
    "fontSize" = "small"
)

SIDEBAR_STYLE <- list(
    'position'="fixed",
    'top' = 20,
    'left' = 0,
    'bottom' = "10px",
    'width'="20rem",
    'padding'="5rem 1rem"
)

PLOT4_STYLE <- list(
    'margin-left' = "30rem",
    "width" = "100%",
    "height" = "520px",
    "border" = "0px"
)

SPOTIFY_LOGO <- "assets/img/spotify-ex.png"
# Navbars (top) and footer (bottom)
# layout for top
navbar <- dbcNavbar(
    dbcContainer(
        list(
            htmlA(
                dbcRow(
                    list(
                        dbcCol(htmlImg(src=SPOTIFY_LOGO, height="50px")),
                        dbcCol(dbcNavbarBrand("Spotify Explorer", className="py-10"))
                    ),
                    align="center",
                    className="g-0",
                    style=NAV_STYLE
                )
            )
        )
    )
)
# layout for footer
container <- dbcContainer(
    list(
        htmlBr(),
        htmlFooter(
            list(
                "(C) Copyright MIT License: Wenxin Xiang.  ",
                paste0("Last time updated on ",{Sys.Date()})
            ),
            style = FOOTER_STYLE
        )
    )
)


# Song characteristics section

# layout for sidebar in Song characteristics
sidebar_widgets <- dbcCol(
    children = list(
        htmlH2("Explore music characteristics", className="display-30"),
        htmlBr(),
        htmlH5("Music Features:"),
        dccDropdown(
            id = "xcol-widget",
            style = list("border-width" = "0", "width" = "100%"),
            options = list(
                list(label = "Danceability", value = "danceability"),
                list(label = "Energy", value = "energy"),
                list(label = "Loudness", value = "loudness"),
                list(label = "Acousticness", value = "acousticness"),
                list(label = "Speechiness", value = "speechiness"),
                list(label = "Instrumentalness", value = "instrumentalness"),
                list(label = "Liveness", value = "liveness"),
                list(label = "Valence", value = "valence"),
                list(label = "Tempo", value = "tempo"),
                list(label = "Duration (min)", value = "Duration (min)")
            ),
            value = "danceability"
        ),
        htmlBr(),
        
        htmlH5("Music Genres:"),
        dccDropdown(
            id = "genres",
            style = list("border-width" = "0", "width" = "100%"),
            options = list(
                list(
                    label = "Electronic dance music",
                    value = "electronic dance music"
                ),
                list(label = "Pop", value = "pop"),
                list(label = "Rap", value = "rap"),
                list(label = "Rock", value = "rock"),
                list(label = "Latin", value = "latin"),
                list(label = "R&B", value = "r&b")
            ),
            value = "electronic dance music",
        )
    ),
    style = SIDEBAR_STYLE
)
# layout for plot_4 in Song characteristics
plot_4_settings <- dbcCol(
    list(
        htmlH3(
            "Song characteristics distribution between two popularity classes",
            className = "display-30"
        ),
        dccGraph(
            id="pop_unpop_id",
            style=list(
                "width" = "80%",
                "height" = "450px",
                "border" = "0px"
            )
        )
    ),
    style = PLOT4_STYLE
)

get_popularity_section <- htmlDiv(list(dbcRow(children=list(sidebar_widgets, plot_4_settings))))

app$layout(
    htmlDiv(
        list(
            navbar, 
            container,
            get_popularity_section
        ), 
        style = list("backgroundColor" = "#eeeeef")
    )
)
# Plots -------------

## Plot4

#' Plot density plot of song characteristics distribution with two popularity classes
#' 
#' @param genre genre of songs
#' @param feat song features to explore on x-axis
#' @return a ggplot showing the distribution

app$callback(
    output("pop_unpop_id", "figure"),
    list(
        input("genres", "value"),
        input("xcol-widget", "value")
        ),
    function(genre, feat) {

        data_pop <- df
        data_pop$`Duration (min)` <- data_pop$duration_ms / 60000
        data_pop$`Popularity class` = if_else(
            data_pop$track_popularity <= median(data_pop$track_popularity),
            "Not popular",
            "Popular"
        )
        data_pop$Genres <- data_pop$playlist_genre
        data_pop$Genres <- replace(data_pop$Genres, 
                    data_pop$Genres == "edm", 
                    "electronic dance music")
        data_pop_query <- data_pop %>%
            filter(Genres == genre)
        plot <- ggplot(data_pop_query) +
            aes(x = !!sym(feat),
                color = `Popularity class`) +
            geom_density() +
            labs(x = str_to_title(feat)) +
            theme(
                text = element_text(size = 14)
            )
        ggplotly(plot)
    }
)

app$run_server(host = '0.0.0.0')
