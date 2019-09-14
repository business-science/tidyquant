.onAttach <- function(libname, pkgname) {

    bsu_rule_color <- "#2c3e50"
    bsu_main_color <- "#1f78b4"

    # Check Theme: If Dark, Update Colors
    if (rstudioapi::isAvailable()) {
        theme <- rstudioapi::getThemeInfo()
        if (theme$dark) {
            bsu_rule_color <- "#7FD2FF"
            bsu_main_color <- "#18bc9c"
        }
    }

    bsu_main <- crayon::make_style(bsu_main_color)

    msg <- paste0(
        cli::rule(left = "Need to Learn tidyquant?", col = bsu_rule_color, line = 2),
        bsu_main('\nBusiness Science offers a 1-hour course through Learning Labs PRO - Lab #9!\n'),
        bsu_main('</> Learn more at: https://university.business-science.io/p/learning-labs-pro </>')
    )

    packageStartupMessage(msg)

}
