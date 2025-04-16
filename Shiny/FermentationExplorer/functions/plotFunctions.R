# Functions for Creating Plots in App
# This script contains functions for generating and customizing different types of plots,
# including summary plots, heatmaps, treemaps, metabolic networks, phylogenetic tree, and scatterplots.
# The functions utilize the Plotly library to make plots interactive. 
# Author: Timothy Hackmann
# Date: 14 October 2024

# --- Plot summary plot ---
  #' Generate Color Palette
  #' 
  #' This function generates a color palette with a specified number of colors, using a gradient between the provided minimum and maximum colors.
  #' 
  #' @param n_colors The number of colors to generate in the palette.
  #' @param min_color The color to use for the minimum value in the gradient.
  #' @param max_color The color to use for the maximum value in the gradient.
  #' @return A vector of color values in hexadecimal format.
  #' @export
  #' @importFrom grDevices colorRampPalette
  generate_color_palette <- function(n_colors, min_color = "black", max_color = "green") {
    colorRampPalette(c(min_color, max_color))(n_colors)
  }
  
  #' Extract Axis Settings
  #' 
  #' This function extracts settings for a specific axis (x, y, or z) from a list of arguments, with default settings provided.
  #' 
  #' @param axis_prefix The prefix indicating the axis ("x", "y", or "z").
  #' @param args A list of arguments containing axis settings.
  #' @param default_settings A list of default settings for the axis.
  #' @return A list of settings for the specified axis.
  #' @export
  extract_axis_settings <- function(axis_prefix, args, default_settings) {
    axis_settings <- default_settings
    
    # Override with user-defined settings
    for (setting in names(default_settings)) {
      #Extract settings for a specific axis (x, y, z)
      arg_name <- paste0(setting, ".", axis_prefix)
      
      if(!is.null(args[[arg_name]])){
        axis_settings[[setting]] <- args[[arg_name]]
      }else{
        #If settings do not match a specific axis, extract settings with no axis specified
        arg_name <- setting
        if (!is.null(args[[arg_name]])){
          axis_settings[[setting]] <- args[[arg_name]]
        }
      }
    }
    return(axis_settings)
  }
  
  #' Shorten Labels
  #'
  #' This function shortens labels to a specified maximum length, adding an ellipsis ("...") if the label is truncated.
  #'
  #' @param labels A vector of labels to be shortened.
  #' @param max_label_length The maximum length for each label. Labels longer than this will be truncated.
  #' @return A vector of shortened labels, retaining factor status if input was a factor.
  #' @export
  #' @importFrom stringr str_sub
  shorten_labels <- function(labels, max_label_length = 15) {
    was_factor <- is.factor(labels) # Check if input is a factor
    
    labels <- as.character(labels) # Convert to character if it's a factor
    
    if (!is.null(max_label_length)) {
      short_labels <- sapply(labels, function(label) {
        if (nchar(label) > max_label_length) {
          paste0(substr(label, 1, max_label_length), "...")
        } else {
          label
        }
      })
    } else {
      short_labels <- labels
    }
    
    if (was_factor) {
      short_labels <- factor(short_labels, levels = unique(short_labels)) # Convert back to factor
    }
    
    return(short_labels)
  }
  
  
  #' Generate Axis Settings
  #' 
  #' This function generates axis settings for Plotly layouts, allowing customization of tick labels, tick lengths, and axis titles.
  #' 
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale.
  #' @param include_z A logical value indicating whether to include z-axis settings (for 3D plots).
  #' @param ... Additional arguments for customizing axis settings.
  #' @return A list of axis settings for the Plotly layout.
  #' @export
  #' @importFrom plotly layout
  # Helper function to generate axis settings for plotly layout
  generate_axis_settings <- function(coord_fixed = FALSE, x_to_y_ratio = 1, include_z = FALSE, ...) {
    args <- list(...)
    
    # Define default settings
    default_settings <- list(
      title = FALSE,
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = TRUE,
      ticklen = 4,
      showline = FALSE,
      autorange = NULL,
      side = NULL,
      tickangle = NULL,
      type = NULL,          
      categoryorder = NULL,   
      categoryarray = NULL        
    )
    
    # Extract settings for each axis
    xaxis_settings <- extract_axis_settings("x", args, default_settings)
    yaxis_settings <- extract_axis_settings("y", args, default_settings)
    zaxis_settings <- if (include_z) extract_axis_settings("z", args, default_settings) else NULL
    
    # Remove NULL entries to allow default behavior
    xaxis_settings <- xaxis_settings[!sapply(xaxis_settings, is.null)]
    yaxis_settings <- yaxis_settings[!sapply(yaxis_settings, is.null)]
    if (include_z) {
      zaxis_settings <- zaxis_settings[!sapply(zaxis_settings, is.null)]
    }
    
    # Fix x-y coordinates
    if (coord_fixed) {
      xaxis_settings$scaleanchor <- "y"
      xaxis_settings$scaleratio <- x_to_y_ratio
      xaxis_settings$constrain <- "domain"
      yaxis_settings$scaleanchor <- "x"
      yaxis_settings$scaleratio <- 1 / x_to_y_ratio
      yaxis_settings$constrain <- "domain"
    }
    
    # Return settings
    if (include_z) {
      return(list(xaxis = xaxis_settings, yaxis = yaxis_settings, zaxis = zaxis_settings))
    } else {
      return(list(xaxis = xaxis_settings, yaxis = yaxis_settings))
    }
  }
  
  #' Add Categorical Legend
  #'
  #' This  function adds a categorical legend to a Plotly plot. The legend includes color boxes with corresponding labels.
  #'
  #' @param plot A plotly plot object to which the legend will be added.
  #' @param legend_labels A vector of labels to display in the legend.
  #' @param min_color The color to use for the minimum value in the legend.
  #' @param max_color The color to use for the maximum value in the legend.
  #' @param legend_title The title of the legend.
  #' @param legend_box_size The size of the legend boxes.
  #' @param legend_spacing_y The vertical spacing between legend boxes.
  #' @param legend_start_x The x-coordinate where the legend should start.
  #' @param right_margin The width of the right margin to leave for the legend.
  #' @return A plotly plot object with the added legend.
  #' @export
  #' @importFrom plotly layout
  add_categorical_legend <- function(plot, legend_labels, min_color, max_color, legend_title = NULL, legend_box_size = 2, legend_spacing_y = 1, legend_start_x = 1.05, right_margin = 200) {
    n_colors = length(legend_labels)
    
    # Generate color palette directly using provided min and max colors
    palette = generate_color_palette(n_colors = n_colors, min_color = min_color, max_color = max_color)
    
    # Calculate legend position and spacing
    n_boxes <- length(legend_labels)
    total_legend_height <- n_boxes * (legend_box_size + legend_spacing_y) / 100
    center_y <- 0.5  # Vertical center of the plot
    legend_start_y <- center_y + total_legend_height / 2
    
    # Initialize lists for shapes and annotations
    shapes <- list()
    annotations <- list()
    
    # Add title annotation for the vertical legend, centered at the plot
    annotations[[1]] <- list(
      x = legend_start_x,
      y = legend_start_y + (legend_spacing_y / 500),  # Slightly above the first box
      text = legend_title,
      showarrow = FALSE,
      xref = "paper",
      yref = "paper",
      xanchor = "left",
      yanchor = "bottom",
      font = list(size = 14, color = "black"),
      xshift = 0,
      yshift = 0
    )
    
    # Calculate y positions and add boxes and labels for the centered vertical legend
    for (i in 1:n_boxes) {
      y_position <- legend_start_y - ((i - 1) * (legend_spacing_y / 100)) - ((legend_box_size * (i - 1)) / 100)
      
      # Get scale ratio
      xaxis_scaleratio = plot$x$layout$xaxis$scaleratio
      yaxis_scaleratio = plot$x$layout$yaxis$scaleratio
      x_to_y_ratio = xaxis_scaleratio/yaxis_scaleratio
      
      if (length(x_to_y_ratio) == 0) {
        x1_position <- legend_start_x + (legend_box_size / 100)
      } else {
        x1_position <- legend_start_x + (legend_box_size / (100 * x_to_y_ratio))
      }
      
      # Add shape for the legend box
      shapes[[i]] <- list(
        type = "rect",
        xref = "paper",
        yref = "paper",
        x0 = legend_start_x,
        x1 = x1_position,
        y0 = y_position - (legend_box_size / 100),
        y1 = y_position,
        fillcolor = palette[i],
        line = list(color = palette[i])
      )
      
      # Add annotation for the label next to the box
      annotations[[i + 1]] <- list(
        x = x1_position + 0.005,
        y = y_position - (legend_box_size / 200),
        text = legend_labels[i],
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "middle",
        font = list(size = 12),
        xshift = 5  # Fixed pixel padding
      )
    }
    
    # Apply shapes, annotations, and margins to the plot
    plot <- plot %>%
      plotly::layout(
        shapes = shapes,
        annotations = annotations,
        margin = list(r = right_margin)  # Adjust right margin to make room for the legend
      )
    
    return(plot)
  }
  
  #' Create Placeholder Plot with Custom Message
  #' 
  #' This function generates a placeholder plot with a custom message displayed in the center of the plot area.
  #' The message can be used to indicate that no results were found or to provide other information to the user.
  #' 
  #' @param message The message to display in the center of the plot.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x0 The x-coordinate of the lower left corner of the message box.
  #' @param y0 The y-coordinate of the lower left corner of the message box.
  #' @param x1 The x-coordinate of the upper right corner of the message box.
  #' @param y1 The y-coordinate of the upper right corner of the message box.
  #' @param font_size The font size of the message text.
  #' @param font_color The color of the message text.
  #' @param bgcolor The background color of the message box.
  #' @param rect_color The color of the border of the message box.
  #' @param rect_fillcolor The fill color of the message box.
  #' @return A plotly plot object displaying the custom message.
  #' @examples
  #' @export
  #' @importFrom plotly plot_ly layout
  plot_message <- function(message = "No results found",coord_fixed = TRUE,
                           x0 = -0.5, y0 = -0.5, x1 = 0.5, y1 = 0.5,
                           font_size = 20, font_color = "black",
                           bgcolor = "rgba(255, 255, 255, 0.0)", rect_color = "rgba(0, 0, 0, 0.5)",rect_fillcolor = "rgba(0, 0, 0, 0.25)") {
    # Get axis settings
    axis_settings <- generate_axis_settings(
      showticklabels.x = FALSE,
      showticklabels.y = FALSE,
      ticklen.x = 0,
      ticklen.y = 0,
      coord_fixed = coord_fixed
    )
    
    # Create plot
    plot <- plotly::plot_ly(
      type = "scatter",
      mode = "none"  # No markers, lines, or text
    ) %>%
      plotly::layout(
        shapes = list(
          list(
            type = "rect",
            x0 = x0, y0 = y0, x1 = x1, y1 = y1,
            fillcolor = rect_fillcolor,
            line = list(color = rect_color),
            layer = "above"  # Set the rectangle to be above the other elements
          )
        ),
        annotations = list(
          x = (x0 + x1) / 2,  # Center the annotation in the x direction
          y = (y0 + y1) / 2,  # Center the annotation in the y direction
          text = message,
          showarrow = FALSE,
          xref = "x",  
          yref = "y",  
          xanchor = "center",
          yanchor = "middle",
          font = list(size = font_size, color = font_color),
          bgcolor = bgcolor,
          borderpad = 10
        ),
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
    
    return(plot)
  }
  
  #' Make Main Summary Plot
  #' 
  #' This function generates a summary plot, which is a heatmap-style plot that displays a matrix of values with corresponding labels.
  #' The plot can be used to visualize binary or categorical data, with each cell colored based on the value in the matrix.
  #' 
  #' @param df A data frame containing the values to be plotted. The rows represent the y-axis labels, and the columns represent the x-axis labels.
  #' @param title The title of the plot.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale. This parameter is used to adjust the aspect ratio of the plot when coord_fixed is set to TRUE.
  #' @param showlegend A logical value indicating whether to display a color legend for the plot.
  #' @param max_label_length The maximum number of characters to display for each label. Labels longer than this will be truncated.
  #' @param min_color The color to use for the minimum value in the matrix.
  #' @param max_color The color to use for the maximum value in the matrix.
  #' @param hovertemplate The template for the hover text that appears when hovering over a cell in the plot.
  #' @param legend_labels The labels to display in the color legend.
  #' @param legend_title The title of the color legend.
  #' @param legend_box_size The size of the color legend boxes.
  #' @param legend_spacing_y The vertical spacing between color legend boxes.
  #' @param legend_start_x The x-coordinate where the color legend should start.
  #' @param right_margin The width of the right margin to leave for the color legend.
  #' @return A plotly plot object displaying the summary heatmap.
  #' @examples
  #' @export
  #' @importFrom plotly plot_ly add_trace layout
  #' @importFrom dplyr nrow as.data.frame select distinct arrange filter
  plot_summary <- function(df, title = NULL, coord_fixed = TRUE, x_to_y_ratio = 1, showlegend = FALSE, max_label_length = 15, min_color = "#7F7F7F", max_color = "#00B050",
                           hovertemplate = "<b>X: %{x}</b><br><b>Y: %{y}</b><br><extra></extra>",
                           legend_labels = c("0","1"), legend_title = NULL, legend_box_size = 2, legend_spacing_y = 1, legend_start_x = 1.05, right_margin = 200) {
    
    #Check if input data frame is empty and return a message if so
    if (nrow(df) == 0) {
      return(plot_message(message = "No predictions"))
    }
    
    # Get data
    mat <- as.matrix(df)
    
    # Format axis labels
    full_x_labels <- colnames(mat)
    short_x_labels <- shorten_labels(full_x_labels, max_label_length)
    
    # Format cell labels
    cell_labels <- matrix(ifelse(mat > 0, "(+)", "(-)"), nrow = nrow(mat), byrow = TRUE)
    
    # Generate color palette using min and max colors
    palette <- generate_color_palette(n_colors = 2, min_color = min_color, max_color = max_color)
    
    # Set fill colors for main plot
    fill_main <- list(
      list(0, palette[1]),
      list(1, palette[2])
    )
    
    # Set fill colors for background plot
    fill_background <- list(
      list(0, colorspace::lighten(palette[1], amount = -0.3)),
      list(1, colorspace::lighten(palette[2], amount = -0.3))
    )
    
    # Make plot
    plot <- plotly::plot_ly() %>%
      #Add background (provides border)
      plotly::add_trace(
        type = 'heatmap',
        z = mat,
        x = short_x_labels,
        y = "",
        xgap = 2,
        ygap = 2,
        zmin = 0,
        zmax = 100,
        colorscale = fill_background,
        showscale = showlegend,
        text = cell_labels,
        texttemplate = "%{text}",
        textfont = list(color = "white", size = 12),
        hovertemplate = hovertemplate
      ) %>%
      #Add main plot
      plotly::add_trace(
        type = 'heatmap',
        z = mat,
        x = short_x_labels,
        y = "",
        xgap = 5,
        ygap = 5,
        zmin = 0,
        zmax = 100,
        colorscale = fill_main,
        showscale = showlegend,
        text = cell_labels,
        texttemplate = "%{text}",
        textfont = list(color = "white", size = 12),
        hovertemplate = hovertemplate
      )
    
    # Apply layout
    axis_settings <- generate_axis_settings(
      showticklabels.x = TRUE,
      showticklabels.y = TRUE,
      ticklen.x = 4,
      ticklen.y = 0,
      type = "category",   
      categoryorder = "array", 
      categoryarray.x = short_x_labels,
      coord_fixed = coord_fixed
    )
    
    plot <- plot %>%
      plotly::layout(
        title = title,
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
    
    plot <- add_categorical_legend(plot, legend_labels = legend_labels, min_color = min_color, max_color = max_color, legend_title = legend_title, 
                                   legend_box_size = legend_box_size, legend_spacing_y = legend_spacing_y, legend_start_x = legend_start_x, right_margin = right_margin)
    
    return(plot)
  }
  

# --- Plot heatmap ---
  #' Make Main Heatmap Plot
  #' 
  #' This function generates a heatmap plot based on the provided data frame, allowing customization of colors, labels, and layout.
  #' 
  #' @param df A data frame containing the values to be plotted.
  #' @param title The title of the plot.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale.
  #' @param row_threshold The maximum number of rows before adjusting the plot settings.
  #' @param col_threshold The maximum number of columns before adjusting the plot settings.
  #' @param showlegend A logical value indicating whether to display a color legend for the plot.
  #' @param max_label_length The maximum number of characters to display for each label. Labels longer than this will be truncated.
  #' @param min_color The color to use for the minimum value in the matrix.
  #' @param max_color The color to use for the maximum value in the matrix.
  #' @param hovertemplate The template for the hover text that appears when hovering over a cell in the plot.
  #' @param data_are_binary A logical value indicating whether the data are binary (0 or 1).
  #' @param legend_labels The labels to display in the color legend.
  #' @param legend_title The title of the color legend.
  #' @param legend_box_size The size of the color legend boxes.
  #' @param legend_spacing_y The vertical spacing between color legend boxes.
  #' @param legend_start_x The x-coordinate where the color legend should start.
  #' @param right_margin The width of the right margin to leave for the color legend.
  #' @param zmin The minimum value for the color scale.
  #' @param zmax The maximum value for the color scale.
  #' @return A plotly plot object displaying the heatmap.
  #' @export
  #' @importFrom plotly plot_ly add_trace layout hide_colorbar
  #' @importFrom dplyr select filter
  #' @importFrom colorspace lighten darken
  plot_heatmap <- function(df, title = NULL, coord_fixed = TRUE, x_to_y_ratio = nrow(df)/2 + 1/2, row_threshold = 10, col_threshold = 10, showlegend = FALSE, max_label_length = 15, min_color = "black", max_color = "green", 
                           hovertemplate = "<b>X: %{x}</b><br><b>Y: %{y}</b><br><extra></extra>", data_are_binary = FALSE, 
                           legend_labels = c("0","1"), legend_title = NULL, legend_box_size = 2, legend_spacing_y = 1, legend_start_x = 1.05, right_margin = 200,
                           zmin = 0, zmax= 100) {
    #Check if input data frame is empty and return a message if so
    if (nrow(df) == 0) {
      return(plot_message(message = "No predictions"))
    }
    
    # Get plot attributes
    # Format data
    if ("x" %in% colnames(df)) {
      mat <- as.matrix(df %>% dplyr::select(-x))
    } else {
      mat <- as.matrix(df)
    }
    
    # Format axis labels
    full_x_labels <- colnames(mat)
    short_x_labels <- shorten_labels(full_x_labels, max_label_length)
    
    # Set text for hover template
    if(data_are_binary==TRUE)
    {
      text <- matrix(ifelse(mat == 100, "(+)", ifelse(mat == 0, "(-)", as.character(mat))), 
                     nrow = nrow(mat), 
                     ncol = ncol(mat))
    }
    
    # Generate color palette using min and max colors
    palette <- generate_color_palette(n_colors = 2, min_color = min_color, max_color = max_color)
    
    # Set fill colors for main plot
    gradient_fill <- list(
      list(0, palette[1]),
      list(1, palette[2])
    )
    
    # Set fill colors for background plot
    palette_background = generate_color_palette(n_colors = 2, 
                                                min_color = colorspace::lighten(col = min_color, amount = 0.2), 
                                                max_color = colorspace::darken(col = max_color, amount = 0.2))
    
    gradient_fill_background <- list(
      list(0, palette_background[1]),
      list(1, palette_background[2])
    )
    
    # Set borders
    xgap <- ifelse(ncol(mat) > col_threshold, 0, 1)
    ygap <- ifelse(nrow(mat) > row_threshold, 0, 1)
    
    # Make plot
    plot <- plotly::plot_ly() %>%
      #Add background (provides border)
      plotly::add_trace(
        type = 'heatmap',
        z = mat,
        x = short_x_labels,
        y = as.character(df$x),
        colorscale = gradient_fill_background,
        showscale = showlegend,
        zmin = zmin,
        zmax = zmax,
        hoverinfo = "none"
      ) %>%
      #Add main plot
      plotly::add_trace(
        type = 'heatmap',
        z = mat,
        x = short_x_labels,
        y = as.character(df$x),
        colorscale = gradient_fill,
        showscale = showlegend,
        zmin = zmin,
        zmax = zmax,
        xgap = xgap,
        ygap = ygap,
        text = text,
        hovertemplate = hovertemplate
      )
    
    # Apply layout
    axis_settings = generate_axis_settings(
      coord_fixed = coord_fixed,
      x_to_y_ratio = x_to_y_ratio,
      type = "category",   
      categoryorder = "array", 
      categoryarray.x = short_x_labels,
      categoryarray.y = as.character(df$x)
    ) 
    
    plot <- plot %>%
      plotly::layout(
        title = title,
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
    
    # Add categorical legend
    plot <- add_categorical_legend(plot, legend_labels = legend_labels, min_color = min_color, max_color = max_color, legend_title = legend_title, 
                                   legend_box_size = legend_box_size, legend_spacing_y = legend_spacing_y, legend_start_x = legend_start_x, right_margin = right_margin)
    
    return(plot)
  }

# --- Plot treemap ---
  #' Pad Strings to a Given Length
  #'
  #' This helper function pads strings with spaces to achieve a specified target length.
  #'
  #' @param x A vector of strings to be padded.
  #' @param target_length The target length for each string after padding.
  #' @return A vector of padded strings, retaining factor status if input was a factor.
  #' @export
  #' @importFrom stringr str_pad
  pad_to_length <- function(x, target_length = 8) {
    was_factor <- is.factor(x) # Check if input is a factor
    
    x <- as.character(x) # Convert to character if it's a factor
    
    padded_strings <- sapply(x, function(str) {
      n <- nchar(str)
      if (n >= target_length) {
        return(str)
      }
      padding <- (target_length - n) / 2
      left_padding <- floor(padding)
      right_padding <- ceiling(padding)
      paste0(strrep(" ", left_padding), str, strrep(" ", right_padding))
    })
    
    if (was_factor) {
      padded_strings <- factor(padded_strings, levels = unique(padded_strings)) # Convert back to factor
    }
    
    return(padded_strings)
  }

  #' Make Main Treemap Plot
  #' 
  #' This function generates a treemap plot based on the provided data frame, allowing customization of labels, colors, and layout.
  #' 
  #' @param df A data frame containing the values to be plotted.
  #' @param title The title of the plot.
  #' @param max_colors The maximum number of colors to use in the treemap.
  #' @param hovertemplate The template for the hover text that appears when hovering over a cell in the plot.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale.
  #' @return A plotly plot object displaying the treemap.
  #' @export
  #' @importFrom plotly plot_ly add_trace layout
  #' @importFrom colorspace rainbow_hcl
  plot_treemap <- function(df, title = NULL, max_colors = 8, 
                            hovertemplate = "<b>X: %{x}</b><br><b>Y: %{y}</b><br><extra></extra>",
                            coord_fixed = FALSE, x_to_y_ratio = 1,
                            width, height) {
    #Check if input data frame is empty and return a message if so
    if (nrow(df) == 0) {
      return(plot_message(message = "No predictions"))
    }
    
    # Get plot attributes
    # Format data
    df$y <- sapply(df$y, pad_to_length, target_length = 10)
    
    # Set colors
    num_colors <- min(nrow(df), max_colors)
    colors <- colorspace::rainbow_hcl(num_colors)
    df$color <- rep(colors, length.out = nrow(df))
    
    # Make plot
    plot <- plotly::plot_ly() %>%
      plotly::add_trace(
        type = 'treemap',
        ids = df$y,
        labels = df$y,
        parents = "",
        values = df$z,
        textinfo = 'label',
        hovertemplate = hovertemplate,
        marker = list(colors = df$color, line = list(width = 2, color = 'black')),
        insidetextfont = list(size = 40), 
        outsidetextfont = list(size = 0),  
        textposition = 'middle center',
        tiling = list(pad = 0)
      )
    
    # Apply layout
    axis_settings = generate_axis_settings() 
    
    # **Suppress the deprecation warning when setting layout**
    plot <- plot %>% 
      plotly::layout(
        title = title,
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        width = width,
        height = height
      )
    
    return(plot)
  }    
  
# --- Plot metabolic network ---
  #' Extract Graph Attributes
  #' 
  #' This helper function extracts vertex and edge attributes from an igraph object, returning them as a list.
  #' 
  #' @param graph An igraph object from which to extract attributes.
  #' @return A list containing vertex and edge attributes.
  #' @export
  #' @importFrom igraph vertex_attr edge_attr
  extract_graph_attr <- function(graph) {
    # Vertex attributes with defaults
    vertex_attr_defaults <- list(
      size = 15,
      size2 = 15,
      color = "SkyBlue2",
      frame.color = "black",
      frame.width = 1,
      shape = "circle",
      opacity = 1,
      name = NA,
      label = NA,
      label.family = "serif",
      label.font = 1,
      label.cex = 1,
      label.dist = 0,
      label.degree = -pi/4,
      label.color = "black"
    )
    
    # Edge attributes with defaults
    edge_attr_defaults <- list(
      color = "darkgrey",
      width = 1,
      opacity = 1,
      arrow.size = 1,
      arrow.width = 1,
      lty = 1,
      label = NA,
      label.family = "serif",
      label.font = 1,
      label.cex = 1,
      label.color = "black"
    )
    
    # Extract vertex attributes
    vertices <- lapply(names(vertex_attr_defaults), function(attr) {
      if (is.null(igraph::vertex_attr(graph, attr))) {
        rep(vertex_attr_defaults[[attr]], igraph::vcount(graph))
      } else {
        igraph::vertex_attr(graph, attr)
      }
    })
    names(vertices) <- names(vertex_attr_defaults)
    
    # Extract edge attributes
    edges <- lapply(names(edge_attr_defaults), function(attr) {
      if (is.null(igraph::edge_attr(graph, attr))) {
        rep(edge_attr_defaults[[attr]], igraph::ecount(graph))
      } else {
        igraph::edge_attr(graph, attr)
      }
    })
    names(edges) <- names(edge_attr_defaults)
    
    list(vertices = vertices, edges = edges)
  }
  
  #' Get Edge Data from igraph Object
  #' 
  #' This helper function retrieves data for edges from an igraph object, including coordinates and attributes.
  #' 
  #' @param graph An igraph object representing the network.
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @param spread The spread of the network, used to adjust the layout.
  #' @return A data frame containing edge data, including coordinates and attributes.
  #' @export
  #' @importFrom dplyr bind_cols
  get_edge_data = function(graph, layout, spread = 0.1)
  {
    # Get attributes from graph
    attributes = extract_graph_attr(graph)
    edge_data <- as.data.frame(attributes$edges)
    
    # Adjust edge color for opacity (opacity not directly supported)
    edge_data$color <- mapply(hex_to_rgba, hex_color = edge_data$color, alpha = edge_data$opacity, SIMPLIFY = TRUE)
    
    # Get  x, y, and z coordinates
    edge_coordinates <- generate_edge_coordinates(graph = graph, layout = layout, spread = spread)
    edge_data = cbind(edge_coordinates, edge_data) 
    
    return(edge_data)
  }
  
  #' Get Vertex Data from igraph Object
  #' 
  #' This helper function retrieves data for vertices from an igraph object, including coordinates and attributes.
  #' 
  #' @param graph An igraph object representing the network.
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @return A data frame containing vertex data, including coordinates and attributes.
  #' @export
  #' @importFrom dplyr bind_cols
  get_vertex_data = function(graph, layout)
  {
    # Get attributes from graph
    attributes = extract_graph_attr(graph)
    vertex_data <- as.data.frame(attributes$vertices)
    
    # Get x, y, and z coordinates of vertices and edges
    vertex_coordinates <- if (ncol(layout) == 2) {
      data.frame(x = layout[, 1], y = layout[, 2])
    } else if (ncol(layout) == 3) {
      data.frame(x = layout[, 1], y = layout[, 2], z = layout[, 3])
    }
    vertex_data = cbind(vertex_coordinates, vertex_data)
    
    
    return(vertex_data)
  }
  
  #' Convert igraph Object to Plotly Plot
  #' 
  #' This function converts an igraph object to a Plotly plot, allowing customization of the layout, colors, and labels.
  #' 
  #' @param graph An igraph object representing the network.
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @param spread The spread of the network, used to adjust the layout.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale.
  #' @param showlegend A logical value indicating whether to display a legend for the plot.
  #' @param showlabels A logical value indicating whether to display labels for the nodes.
  #' @return A plotly plot object displaying the network.
  #' @export
  #' @importFrom plotly plot_ly add_trace layout add_segments add_text
  #' @importFrom dplyr distinct filter arrange
  igraph_to_plotly <- function(graph, layout, spread = 0.1, coord_fixed = FALSE, x_to_y_ratio = 1, showlegend = FALSE, showlabels = TRUE) {
    # Get attributes for plot
    if (!inherits(graph, "igraph")) {
      stop("Please provide a graph as an igraph object.")
    }
    
    vertex_data <- get_vertex_data(graph, layout)
    edge_data <- get_edge_data(graph, layout, spread = spread)
    
    # Set groups for plotting
    vertex_groups <- vertex_data %>% dplyr::distinct(opacity)
    vertex_groups = vertex_groups %>% dplyr::arrange(opacity)
    
    edge_groups <- edge_data %>% dplyr::distinct(color, width, opacity)
    edge_groups = edge_groups %>% dplyr::arrange(opacity)
    
    # Make plot
    plot <- plotly::plot_ly() 
    
    # Add edges   
    ## Add by groups of attributes
    for (i in 1:nrow(edge_groups)) {
      filtered <- edge_data %>%
        dplyr::filter(color == edge_groups$color[i], width == edge_groups$width[i], opacity == edge_groups$opacity[i])
      
      trace_name <- paste0("edge_", i)
      
      #2D plots
      if (ncol(layout) == 2) {
        x_coords <- unlist(lapply(1:nrow(filtered), function(j) c(filtered$x1[j], filtered$xc[j], filtered$x2[j], NA)))
        y_coords <- unlist(lapply(1:nrow(filtered), function(j) c(filtered$y1[j], filtered$yc[j], filtered$y2[j], NA)))
        
        plot <- plot %>%
          plotly::add_trace(
            x = x_coords,
            y = y_coords,
            type = 'scatter',
            mode = 'lines',
            line = list(shape = "spline", color = edge_groups$color[i], width = edge_groups$width[i]),
            name =  trace_name,
            hoverinfo = 'none'
          )
      } else if (ncol(layout) == 3) {
        #3D plots
        x_coords <- unlist(lapply(1:nrow(filtered), function(j) c(filtered$x1[j], filtered$xc[j], filtered$x2[j], NA)))
        y_coords <- unlist(lapply(1:nrow(filtered), function(j) c(filtered$y1[j], filtered$yc[j], filtered$y2[j], NA)))
        z_coords <- unlist(lapply(1:nrow(filtered), function(j) c(filtered$z1[j], filtered$zc[j], filtered$z2[j], NA)))
        
        plot <- plot %>%
          plotly::add_trace(
            x = x_coords,
            y = y_coords,
            z = z_coords,
            type = 'scatter3d',
            mode = 'lines',
            line = list(shape = "spline", color = edge_groups$color[i], width = edge_groups$width[i]),
            name =  trace_name,
            hoverinfo = 'none'
          )
      }
    }
    
    # Add vertices
    ## Add by groups of attributes
    for (i in 1:nrow(vertex_groups)) {
      filtered <- vertex_data %>%
        dplyr::filter(opacity == vertex_groups$opacity[i])
      
      trace_name <- paste0("vertex_", i)
      
      #2D plots
      if (ncol(layout) == 2) {
        plot <- plot %>%
          plotly::add_trace(x = filtered$x,
                            y = filtered$y,
                            type = 'scatter', 
                            mode = 'markers', 
                            marker = list(symbol = 'circle', 
                                          size = filtered$size,
                                          color = filtered$color, 
                                          line = list(color = filtered$frame.color, width = filtered$frame.width),
                                          opacity = vertex_groups$opacity[i]
                            ),
                            name =  trace_name,
                            text = filtered$name, hoverinfo = 'text') 
        if(showlabels){
          plot <- plot %>%
            plotly::add_text(
              x = filtered$x,
              y = filtered$y,
              z = filtered$z,
              text = filtered$label,
              textposition = 'top center',
              showlegend = FALSE
            )
        }
      } else if (ncol(layout) == 3) {
        #3D plots
        plot <- plot %>%
          plotly::add_trace(x = filtered$x,
                            y = filtered$y,
                            z = filtered$z,
                            type = 'scatter3d', mode = 'markers', 
                            marker = list(symbol = 'circle', 
                                          size = filtered$size, 
                                          color = filtered$color, 
                                          line = list(color =  filtered$frame.color, width =  filtered$frame.width),
                                          opacity = vertex_groups$opacity[i]
                            ),
                            name =  trace_name,
                            text = filtered$name, hoverinfo = 'text') 
        if(showlabels){
          plot <- plot %>%
            plotly::add_text(
              x = filtered$x,
              y = filtered$y,
              z = filtered$z,
              text = filtered$label,
              textposition = 'top center',
              showlegend = FALSE
            )
        }
      }
    }
    
    # Apply layout
    if (ncol(layout) == 2) {
      axis_settings = generate_axis_settings(
        ticklen.x = 0,
        ticklen.y = 0,
        showticklabels.x = FALSE,
        showticklabels.y = FALSE,
        title.x = "",
        title.y = "",
        coord_fixed = coord_fixed
      )
      
      plot <- plot %>%
        plotly::layout(
          showlegend = showlegend,
          xaxis = axis_settings$xaxis,
          yaxis = axis_settings$yaxis,
          margin = list(t = 100),
          hovermode = 'closest'
        )
    } else if (ncol(layout) == 3) {
      axis_settings = generate_axis_settings(
        include_z = TRUE,
        showgrid.x = TRUE,
        showgrid.y = TRUE,
        showgrid.z = TRUE,
        showline.x = TRUE,
        showline.y = TRUE,
        showline.z = TRUE,
        showticklabels.x = FALSE,
        showticklabels.y = FALSE,
        showticklabels.z = FALSE,
        coord_fixed = coord_fixed
      )
      
      plot <- plot %>%
        plotly::layout(
          scene = list(
            showlegend = showlegend,
            xaxis = axis_settings$xaxis,
            yaxis = axis_settings$yaxis,
            zaxis = axis_settings$zaxis
          ),
          margin = list(t = 100),
          hovermode = 'closest'
        )
    }
    
    return(plot)
  }
  
  #' Convert Hex Color to RGBA
  #' 
  #' This helper function converts a hexadecimal color code to an RGBA color code, allowing for alpha transparency.
  #' 
  #' @param hex_color A hexadecimal color code to be converted.
  #' @param alpha A numeric value between 0 and 1 representing the alpha transparency.
  #' @return A string representing the color in RGBA format.
  #' @export
  #' @importFrom grDevices col2rgb
  hex_to_rgba <- function(hex_color, alpha = 1) {
    rgb <- col2rgb(hex_color)
    sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
  }
  
  #' Generate Control Points for Bezier Curves
  #' 
  #' This helper function generates control points for Bezier curves in 2D or 3D space, used for plotting curved edges in a network plot.
  #' 
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @param v1 The index of the first node in the edge.
  #' @param v2 The index of the second node in the edge.
  #' @param n The total number of edges between the two nodes.
  #' @param index The index of the current edge among the n edges.
  #' @param spread The spread of the network, used to adjust the layout.
  #' @return A vector representing the coordinates of the control point.
  #' @export
  control_points <- function(layout, v1, v2, n, index, spread = 0.1) {
    midpoint <- (layout[v1, ] + layout[v2, ]) / 2
    
    if (ncol(layout) == 2) {
      angle <- atan2(layout[v2, 2] - layout[v1, 2], layout[v2, 1] - layout[v1, 1]) + pi / 2
      control_dist <- spread * sqrt(sum((layout[v1, ] - layout[v2, ])^2)) * (index - (n / 2))
      control_point <- midpoint + control_dist * c(cos(angle), sin(angle))
      return(control_point)
    } else if (ncol(layout) == 3) {
      vector <- layout[v2, ] - layout[v1, ]
      
      perp_vector1 <- if (vector[3] == 0) {
        c(-vector[2], vector[1], 0)
      } else {
        c(-vector[2], vector[1], 0)
      }
      perp_vector1 <- perp_vector1 / sqrt(sum(perp_vector1^2))
      
      angle <- 2 * pi / n * (index - 1)
      control_point <- midpoint + spread * sqrt(sum((layout[v1, ] - layout[v2, ])^2)) * (cos(angle) * perp_vector1)
      
      return(control_point)
    }
  }
  
  #' Generate Edge Coordinates
  #' 
  #' This helper function generates coordinates for edges in a network plot, including control points for Bezier curves in 2D or 3D space.
  #' 
  #' @param graph An igraph object representing the network.
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @param spread The spread of the network, used to adjust the layout.
  #' @return A data frame containing coordinates for edges, including control points for Bezier curves.
  #' @export
  #' @importFrom dplyr bind_cols select filter
  #' @importFrom igraph as_data_frame V
  generate_edge_coordinates <- function(graph, layout, spread = 0.1) {
    df <- igraph::as_data_frame(graph)
    
    edges_coords <- lapply(1:nrow(df), function(i) {
      layout_df <- as.data.frame(layout)
      names(layout_df) <- if (ncol(layout) == 2) c("x", "y") else c("x", "y", "z")
      layout_df$id <- igraph::V(graph)$name
      layout <- as.matrix(layout_df[, -ncol(layout_df)])
      rownames(layout) <- layout_df$id
      
      edges <- df %>% dplyr::select(to, from)
      
      v1 <- as.character(edges[i, "from"])
      v2 <- as.character(edges[i, "to"])
      edge_pair <- which((edges[, "from"] == v1 & edges[, "to"] == v2) | (edges[, "to"] == v1 & edges[, "from"] == v2))
      n <- length(edge_pair)
      index <- which(edge_pair == i)
      
      control_pt <- control_points(layout, v1, v2, n, index, spread)
      
      if (ncol(layout) == 2) {
        return(data.frame(
          x1 = layout[v1, "x"],
          xc = control_pt[1],
          x2 = layout[v2, "x"],
          y1 = layout[v1, "y"],
          yc = control_pt[2],
          y2 = layout[v2, "y"]
        ))
      } else if (ncol(layout) == 3) {
        return(data.frame(
          x1 = layout[v1, "x"],
          xc = control_pt[1],
          x2 = layout[v2, "x"],
          y1 = layout[v1, "y"],
          yc = control_pt[2],
          y2 = layout[v2, "y"],
          z1 = layout[v1, "z"],
          zc = control_pt[3],
          z2 = layout[v2, "z"]
        ))
      }
    })
    
    do.call(rbind, edges_coords)
  }
  
  #' Get Trace Attributes from Plotly Object
  #' 
  #' This helper function retrieves attributes (e.g., line color and width) from the traces in a Plotly plot.
  #' 
  #' @param plot A plotly plot object from which to extract trace attributes.
  #' @return A data frame containing the names, line colors, and line widths of the traces.
  #' @export
  #' @importFrom plotly plotly_build
  get_trace_attributes <- function(plot) {
    # Ensure the plotly object is properly built
    if (is.null(plot$x$data)) {
      plot <- plotly::plotly_build(plot)
    }
    
    # Initialize lists to store attributes
    trace_names <- c()
    trace_colors <- c()
    trace_widths <- c()
    
    # Iterate over the traces in the plot and extract attributes
    for (trace in plot$x$data) {
      # Handle missing name
      trace_names <- c(trace_names, if (!is.null(trace$name)) trace$name else NA)
      
      # Handle missing line attributes
      if (!is.null(trace$line)) {
        trace_colors <- c(trace_colors, if (!is.null(trace$line$color)) trace$line$color else NA)
        trace_widths <- c(trace_widths, if (!is.null(trace$line$width)) trace$line$width else NA)
      } else {
        trace_colors <- c(trace_colors, NA)
        trace_widths <- c(trace_widths, NA)
      }
    }
    
    # Combine the extracted attributes into a dataframe
    attributes_df <- data.frame(
      name = trace_names,
      line_color = trace_colors,
      line_width = trace_widths,
      stringsAsFactors = FALSE
    )
    
    return(attributes_df)
  }
  
  #' Find Matching Trace
  #' 
  #' This helper function finds a trace in a Plotly plot that matches a given color and line width.
  #' 
  #' @param color_hex A hexadecimal color code to match.
  #' @param line_width The line width to match.
  #' @param trace_attributes A data frame of trace attributes obtained from a Plotly plot.
  #' @return The name of the matching trace, or NA if no match is found.
  #' @export
  #' @importFrom dplyr filter
  get_matching_trace <- function(color_hex, line_width, trace_attributes) {
    # Convert the hex color to rgba
    query_color <- hex_to_rgba(color_hex)
    query_width = line_width
    
    # Find the matching trace based on line color and width
    match <- trace_attributes %>%
      dplyr::filter(
        line_color == query_color &
          line_width == query_width
      )
    
    # Return the matching trace name
    if (nrow(match) == 1) {
      return(match$name)
    } else {
      return(NA)
    }
  }
  
  #' Generate Network Legend Labels
  #' 
  #' This helper function generates labels for the network legend based on the provided legend key and trace attributes.
  #' 
  #' @param network_legend_key A key used to generate labels for the network legend.
  #' @param trace_attributes A data frame of trace attributes obtained from a Plotly plot.
  #' @return A named vector of legend labels.
  #' @export
  generate_network_legend_labels <- function(network_legend_key, trace_attributes) {
    legend_labels <- c()
    
    for (i in seq_len(nrow(network_legend_key))) {
      matching_trace <- get_matching_trace(network_legend_key$line_color[i], network_legend_key$line_width[i], trace_attributes)
      
      # If a match is found, add it to the legend_labels vector
      if (!is.na(matching_trace)) {
        legend_labels[network_legend_key$name[i]] <- matching_trace
      }
    }
    
    return(legend_labels)
  }
  
  #' Reorder Network Legend
  #' 
  #' This function reorders the legend items in a Plotly network plot based on the provided labels.
  #' 
  #' @param plot A plotly plot object representing the network.
  #' @param legend_labels A named vector of labels used to reorder the legend items.
  #' @return A plotly plot object with the reordered legend.
  #' @export
  #' @importFrom plotly plotly_build add_trace layout
  reorder_network_legend <- function(plot, legend_labels) {
    # Check if the plotly object is properly built
    if (is.null(plot$x$data)) {
      plot <- plotly::plotly_build(plot)
    }
    
    # Extract the trace names from the plotly object
    trace_names <- sapply(plot$x$data, function(trace) trace$name)
    
    # Initialize a list to store reordered traces
    reordered_traces <- list()
    
    # Track indices of reordered traces
    reordered_indices <- integer(0)
    
    # Reorder and rename traces according to legend_labels
    for (i in seq_along(legend_labels)) {
      trace_index <- which(trace_names == legend_labels[i])
      if (length(trace_index) > 0) {
        # Rename the trace
        plot$x$data[[trace_index]]$name <- names(legend_labels)[i]
        # Add the trace to reordered list
        reordered_traces[[length(reordered_traces) + 1]] <- plot$x$data[[trace_index]]
        # Record the index as reordered
        reordered_indices <- c(reordered_indices, trace_index)
      }
    }
    
    # Add back any traces not included in the legend_labels, preserving their original order
    remaining_indices <- setdiff(seq_along(plot$x$data), reordered_indices)
    for (j in remaining_indices) {
      plot$x$data[[j]]$showlegend <- FALSE
      reordered_traces[[length(reordered_traces) + 1]] <- plot$x$data[[j]]
    }
    
    # Assign the reordered traces back to the plot
    plot$x$data <- reordered_traces
    
    return(plot)
  }
  
  #' Generate Annotations for Network Plot
  #' 
  #' This helper function generates annotations (e.g., labels and arrows) for nodes in a network plot.
  #' 
  #' @param graph An igraph object representing the network.
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @param label_color The color of the node labels.
  #' @param arrow_color The color of the arrows used for edges.
  #' @param font_size The font size of the node labels.
  #' @param bgcolor_opacity The opacity of the background color for the node labels.
  #' @return A list of annotations for the Plotly layout.
  #' @export
  generate_annotations <- function(graph, layout, label_color = "red", arrow_color = "red", font_size = 10, bgcolor_opacity = 0.6) {
    # Extract graph attributes
    vertex_data <- get_vertex_data(graph, layout)
    
    annotations_list <- vector("list", nrow(vertex_data))
    
    for (i in 1:nrow(vertex_data)) {
      if (!is.na(vertex_data$label[i])){
        annotation <- list(
          x = vertex_data$x[i],
          y = vertex_data$y[i],
          text = vertex_data$label[i],
          font = list(color = label_color, size = font_size),
          bgcolor = paste0("rgba(255, 255, 255, ", bgcolor_opacity, ")"),
          showarrow = TRUE,
          arrowhead = 0,
          arrowcolor = arrow_color,
          xanchor = "center",
          yanchor = "bottom",
          ay = -30
        )
        
        if (ncol(layout) == 3) {
          annotation$z <- vertex_data$z[i]
        }
        
        annotations_list[[i]] <- annotation
      }
    }
    
    return(annotations_list)
  }
  
  #' Make Main Network Plot
  #' 
  #' This function generates a network plot based on the provided igraph object and layout, allowing customization of labels, colors, and layout.
  #' 
  #' @param graph An igraph object representing the network.
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @param spread The spread of the network, used to adjust the layout.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param network_legend_key A key used to generate labels for the network legend.
  #' @param showlegend A logical value indicating whether to display a legend for the plot.
  #' @param showlabels A logical value indicating whether to display labels for the nodes.
  #' @param label_color The color of the node labels.
  #' @param arrow_color The color of the arrows used for edges.
  #' @param font_size The font size of the node labels.
  #' @param bgcolor_opacity The opacity of the background color for the node labels.
  #' @return A plotly plot object displaying the network.
  #' @export
  #' @importFrom plotly plot_ly add_trace layout
  #' @importFrom igraph vcount ecount vertex_attr edge_attr as_data_frame
  plot_network <- function(graph, layout, 
                           spread = 0.05, coord_fixed = TRUE, 
                           network_legend_key = NULL,
                           showlegend = TRUE, showlabels = FALSE,
                           label_color = "red", arrow_color = "red", 
                           font_size = 10, bgcolor_opacity = 0.6) {
    #Check if input graph is empty and return a message if so
    if (igraph::vcount(graph) == 0 || igraph::ecount(graph) == 0) {
      return(plot_message(message = "No network built"))
    }
    
    #  Create the plot using igraph_to_plotly
    p <- igraph_to_plotly(
      graph = graph, 
      layout = layout, 
      spread = spread, 
      coord_fixed = coord_fixed, 
      showlegend = showlegend, 
      showlabels = showlabels
    )
    
    # Get trace attributes for legend reordering
    trace_attributes <- get_trace_attributes(p)
    
    # Generate legend labels
    legend_labels <- generate_network_legend_labels(
      network_legend_key = network_legend_key, 
      trace_attributes = trace_attributes
    )
    
    # Reorder the network legend
    p <- reorder_network_legend(p, legend_labels)
    
    # Add annotations to the network plot
    annotations <- generate_annotations(
      graph = graph, 
      layout = layout, 
      label_color = label_color, 
      arrow_color = arrow_color, 
      font_size = font_size, 
      bgcolor_opacity = bgcolor_opacity
    )
    
    # Apply annotations
    if (ncol(layout) == 2) {
      p <- p %>% plotly::layout(annotations = annotations)
    } else if (ncol(layout) == 3) {
      p <- p %>% plotly::layout(scene = list(annotations = annotations))
    }
    
    return(p)
  }

# --- Plot phylogenetic tree ---
  #' Get Nodes from Tips to Root
  #' 
  #' This function retrieves the nodes along the path from each tip to the root in a phylogenetic tree.
  #' 
  #' @param tree A phylogenetic tree object.
  #' @param layout A data frame specifying the layout coordinates for the nodes (optional).
  #' @return A data frame containing the parent-child node pairs for each tip in the tree.
  #' @export
  #' @importFrom ape nodepath getMRCA as.phylo
  get_nodes_to_root <- function(tree = NULL, layout = NULL) {
    # Check if tree or layout is provided
    if (!is.null(tree)) {
      tree <- tree
    } else if (is.null(tree) & !is.null(layout)) {
      tree <- ape::as.phylo(layout)
    } else {
      stop("Please provide either a tree or a layout")
    }
    
    # Find the root node
    root <- ape::getMRCA(tree, 1:length(tree$tip.label))
    
    # Get the tips from the tree
    tips <- 1:length(tree$tip.label)  # Tips are usually numbered 1 to N
    
    # Initialize an empty dataframe to store results
    result <- data.frame()
    
    # Loop over each tip and find the path to the root
    for (i in 1:length(tips)) {
      nodes_to_root <- ape::nodepath(phy = tree, from = tips[i], to = root)
      
      # Get the tip label and tip node
      tip_label <- tree$tip.label[i]
      tip_node <- nodes_to_root[1]
      
      # Loop over the nodes to create parent-child pairs
      for (j in 1:(length(nodes_to_root) - 1)) {
        parent_child <- data.frame(
          tip_label = tip_label,
          tip_node = tip_node,
          parent_node = nodes_to_root[j + 1],
          child_node = nodes_to_root[j]
        )
        result <- rbind(result, parent_child)
      }
    }
    
    return(result)
  }
  
  #' Get Layout of Phylogenetic Tree
  #' 
  #' This helper function retrieves the layout coordinates of a phylogenetic tree, using ggtree to generate the layout.
  #' 
  #' @param tree A phylogenetic tree object to be converted to a layout.
  #' @param layout_type The type of layout ("rectangular", "circular", etc.).
  #' @return A data frame containing the layout coordinates for the tree.
  #' @export
  #' @importFrom ggtree ggtree
  get_tree_layout = function(tree, layout_type="rectangular")
  {
    p = ggtree::ggtree(tr = tree , layout = layout_type)
    layout = p$data
    
    return(layout)
  }
  
  #' Plot ggtree Object in Plotly
  #' 
  #' This function converts a ggtree object to a Plotly plot, allowing customization of the layout, colors, and labels.
  #' 
  #' @param p A ggtree object to be converted to a Plotly plot.
  #' @param layout A data frame specifying the layout coordinates for the nodes.
  #' @param linewidth The width of the tree branches.
  #' @param color The color of the tree branches.
  #' @param alpha The transparency of the tree branches.
  #' @param type The type of tree layout ("rectangular", "circular", etc.).
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale.
  #' @return A plotly plot object displaying the phylogenetic tree.
  #' @export
  #' @importFrom plotly plot_ly add_segments layout
  ggtree_to_plotly <- function(p = NULL, layout = NULL, linewidth = 1, color = "black", alpha = 1, type="rectangular", coord_fixed = FALSE, x_to_y_ratio = 1) {
    # Get data from ggtree object
    # Get layout
    if (!is.null(layout)) {
      layout <- layout
    } else if (!is.null(p) && inherits(p, "ggtree")) {
      layout <- p$data
    } else if (is.data.frame(p)) {
      layout <- p
    } else {
      stop("Please provide either a ggtree object or a layout dataframe.")
    }
    
    # Extract style attributes from ggtree object
    linewidth <- p$theme$line$linewidth %||% linewidth
    color <- p$theme$line$colour %||% color
    alpha <- p$theme$line$alpha %||% alpha
    
    #Get coordinates
    if(type == "rectangular") {
      # Get coordinates of horizontal lines
      hlines = data.frame(x = rep(NA, nrow(layout)), xend = rep(NA, nrow(layout)), y = rep(NA, nrow(layout)))
      for(i in 1:nrow(hlines)) {
        hlines$x[i] = layout$x[i]
        hlines$xend[i] = layout$x[which(layout$node == layout$parent[i])]
        hlines$y[i] = layout$y[i]
      }
      # Get coordinates of vertical lines
      vlines = data.frame(y = rep(NA, nrow(layout)), yend = rep(NA, nrow(layout)), x = rep(NA, nrow(layout)))
      for(i in 1:nrow(hlines)) {
        vlines$y[i] = layout$y[i]
        vlines$yend[i] = layout$y[which(layout$node == layout$parent[i])]
        vlines$x[i] = hlines$xend[i]
      }
    } else {
      # Get coordinates of splines
      splines = data.frame(x = rep(NA, nrow(layout)), xend = rep(NA, nrow(layout)), y = rep(NA, nrow(layout)), yend = rep(NA, nrow(layout)))
      for(i in 1:nrow(splines)) {
        splines$x[i] = layout$x[i]
        splines$xend[i] = layout$x[which(layout$node == layout$parent[i])]
        splines$y[i] = layout$y[i]
        splines$yend[i] = layout$y[which(layout$node == layout$parent[i])]
      }
    }
    
    # Create plot
    plot <- plotly::plot_ly()
    if(type == "rectangular") {
      plot <- plot %>%
        plotly::add_segments(
          data = hlines, 
          x = ~x, xend = ~xend, y = ~y, yend = ~y,
          line = list(color = color, width = linewidth, opacity = alpha),
          hoverinfo = "skip"
        ) %>%
        plotly::add_segments(
          data = vlines, 
          x = ~x, xend = ~x, y = ~y, yend = ~yend,
          line = list(color = color, width = linewidth),
          hoverinfo = "skip"
        ) 
    } else {
      plot <- plot %>%
        plotly::add_segments(
          data = splines, 
          x = ~x, xend = ~xend, y = ~y, yend = ~yend,
          line = list(color = color, width = linewidth, opacity = alpha),
          hoverinfo = "skip"
        )
    }
    
    # Apply layout
    axis_settings = generate_axis_settings(
      showticklabels.x = FALSE,
      showticklabels.y = FALSE,
      ticklen.x = 0,
      ticklen.y = 0,
      coord_fixed = coord_fixed
    )
    
    plot <- plot %>%
      plotly::layout(
        title = FALSE,
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
    
    return(plot)
  }

# --- Plot scatter plot ---
  #' Add Taxonomy to Graph Layout
  #' 
  #' This helper function adds taxonomy information (e.g., Phylum, Class, Order) to a graph layout based on matching IDs.
  #' 
  #' @param layout A data frame containing the layout coordinates for the nodes.
  #' @param layout_ID The column name in the layout data frame to match with the taxonomy data.
  #' @param taxonomy A data frame containing the taxonomy information.
  #' @param taxonomy_ID The column name in the taxonomy data frame to match with the layout data.
  #' @return A data frame containing the layout coordinates with added taxonomy information.
  #' @export
  #' @importFrom dplyr filter
  add_taxonomy_to_layout <- function(layout, layout_ID = "label", taxonomy, taxonomy_ID = "IMG_Genome_ID_max_genes") {
    row_match <- match(x = layout[[layout_ID]], table = taxonomy[[taxonomy_ID]])
    
    layout$Phylum <- taxonomy$Phylum[row_match]
    layout$Class <- taxonomy$Class[row_match]
    layout$Order <- taxonomy$Order[row_match]
    layout$Family <- taxonomy$Family[row_match]
    layout$Genus <- taxonomy$Genus[row_match]
    layout$Species <- taxonomy$Species[row_match]
    
    layout <- layout %>% dplyr::filter(!!rlang::sym(layout_ID) %in% taxonomy[[taxonomy_ID]])
    return(layout)
  }    
  
  #' Add Fill Color to Graph Layout
  #' 
  #' This helper function applies fill colors to a graph layout based on a specified grouping variable, with optional lightening of the colors.
  #' 
  #' @param layout A data frame containing the layout coordinates for the nodes.
  #' @param group The name of the column in the layout data frame used for grouping.
  #' @param lighten_amount A numeric value indicating the amount by which to lighten the fill colors.
  #' @return A data frame containing the layout coordinates with added fill colors.
  #' @export
  #' @importFrom colorspace qualitative_hcl lighten
  add_fill_to_layout <- function(layout, group, lighten_amount = 0) {
    # Check if layout is empty
    if (nrow(layout) == 0) {
      return(layout)
    }
    
    # Convert the group column to character, clean up non-standard characters, and convert back to factor
    layout[[group]] <- as.character(layout[[group]])
    layout[[group]] <- gsub("[^[:alnum:] [:punct:]]", "", layout[[group]]) # Removes non-alphanumeric characters except standard punctuation
    layout[[group]] <- trimws(layout[[group]]) # Removes leading and trailing whitespace
    layout[[group]] <- as.factor(layout[[group]]) # Convert back to factor
    
    # Get unique levels for the specified group
    group_levels <- sort(unique(layout[[group]]))
    
    # Generate default ggplot2 colors for the group levels
    n = length(group_levels)
    group_colors <- colorspace::qualitative_hcl(n, h = c(15, 375 * (n - 1) / n), c = 100, l = 65, fixup = TRUE, alpha = 1)
    names(group_colors) <- group_levels
    
    # Apply colors and lighten them
    layout$fill <- group_colors[layout[[group]]]
    if (lighten_amount != 0) {
      layout$fill <- colorspace::lighten(layout$fill, amount = lighten_amount)
    }else{
      layout$fill = layout$fill
    }
    
    return(layout)
  }
  
  #' Add Border Color to Graph Layout
  #' 
  #' This helper function applies border colors to a graph layout based on a specified grouping variable, with optional lightening of the colors.
  #' 
  #' @param layout A data frame containing the layout coordinates for the nodes.
  #' @param group The name of the column in the layout data frame used for grouping.
  #' @param lighten_amount A numeric value indicating the amount by which to lighten the border colors.
  #' @return A data frame containing the layout coordinates with added border colors.
  #' @export
  #' @importFrom colorspace qualitative_hcl lighten
  add_color_to_layout <- function(layout, group, lighten_amount = 0) {
    # Check if layout is empty
    if (nrow(layout) == 0) {
      return(layout)
    }
    
    # Convert the group column to character, clean up non-standard characters, and convert back to factor
    layout[[group]] <- as.character(layout[[group]])
    layout[[group]] <- gsub("[^[:alnum:] [:punct:]]", "", layout[[group]]) # Removes non-alphanumeric characters except standard punctuation
    layout[[group]] <- trimws(layout[[group]]) # Removes leading and trailing whitespace
    layout[[group]] <- as.factor(layout[[group]]) # Convert back to factor
    
    # Get unique levels for the specified group
    group_levels <- sort(unique(layout[[group]]))
    
    # Generate default ggplot2 colors for the group levels
    n = length(group_levels)
    group_colors <- colorspace::qualitative_hcl(n, h = c(15, 375 * (n - 1) / n), c = 100, l = 65, fixup = TRUE, alpha = 1)
    names(group_colors) <- group_levels
    
    # Apply colors to the layout based on the cleaned group levels
    layout$color <- group_colors[layout[[group]]]
    
    # Apply lightening if specified
    if (lighten_amount != 0) {
      layout$color <- colorspace::lighten(layout$color, amount = lighten_amount)
    } else {
      layout$color <- layout$color
    }
    
    return(layout)
  }
  
  #' Make Main Scatter Plot
  #' 
  #' This function generates a scatter plot based on the provided data frame, allowing customization of colors, labels, and layout.
  #' 
  #' @param df A data frame containing the x and y coordinates of the points to be plotted.
  #' @param label A vector of labels for the points (optional).
  #' @param color The color of the point borders.
  #' @param fill The fill color of the points.
  #' @param stroke The width of the point borders.
  #' @param size The size of the points.
  #' @param shape The shape of the points ("circle", "square", etc.).
  #' @param alpha The transparency of the points.
  #' @param ticklen.x The length of the x-axis ticks.
  #' @param ticklen.y The length of the y-axis ticks.
  #' @param showticklabels.x A logical value indicating whether to show x-axis tick labels.
  #' @param showticklabels.y A logical value indicating whether to show y-axis tick labels.
  #' @param title.x The title of the x-axis.
  #' @param title.y The title of the y-axis.
  #' @param var_name The name of the variable to be displayed as the plot title.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale.
  #' @param showlegend A logical value indicating whether to display a legend for the plot.
  #' @param max_label_length The maximum number of characters to display for each label. Labels longer than this will be truncated.
  #' @return A plotly plot object displaying the scatter plot.
  #' @export 
  #' @importFrom plotly plot_ly add_markers layout hide_colorbar
  #' @importFrom dplyr select
  plot_scatterplot <- function(df, 
                               label = NULL, color="black", fill="black", stroke=1, size=5, shape="circle", alpha=1, 
                               ticklen.x = 0, ticklen.y = 0, showticklabels.x = FALSE, showticklabels.y = FALSE, title.x = "", title.y = "",
                               var_name = NULL, coord_fixed = FALSE, x_to_y_ratio = 1, showlegend = FALSE, max_label_length = 15) {
    #Check if input data frame is empty and return a message if so
    if (nrow(df) == 0) {
      return(plot_message(message = "No matching organisms", 
                          bgcolor = "rgba(255, 255, 255, 0.25)", rect_color = "rgba(0, 0, 0, 0)",rect_fillcolor = "rgba(0, 0, 0, 0)"))
    }
    
    # Get plot attributes
    #Get hover text
    if(!is.null(label)) {
      label_data <- df %>% dplyr::select(all_of(label))
      hover_text <- apply(label_data, 1, function(row) {
        paste(paste0(names(label_data), ": ", row), collapse = "<br>")
      })
    } else {
      hover_text <- paste0("x: ", df$x, "<br>y: ", df$y)
    }
    
    # Make plot
    plot <- plotly::plot_ly() %>%
      plotly::add_markers(
        data = df,
        x = ~x,
        y = ~y,
        marker = list(
          size = size,
          color = fill,
          symbol = shape,
          line = list(color = color, width = stroke),
          opacity = alpha
        ),
        text = hover_text,
        hoverinfo = 'text'
      )
    
    # Apply layout
    axis_settings = generate_axis_settings(
      coord_fixed = coord_fixed,
      ticklen.x = ticklen.x,
      ticklen.y = ticklen.y,
      showticklabels.x = showticklabels.x,
      showticklabels.y = showticklabels.y,
      title.x = title.x,
      title.y = title.y
    ) 
    
    plot <- plot %>%
      plotly::layout(
        title = var_name,
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
    
    if (!showlegend) {
      plot <- plotly::hide_colorbar(plot)
    }
    
    return(plot)
  }

# --- Make other plots ---
  #' Plot Confusion Matrix
  #'
  #' This function generates a Plotly heatmap to visualize a confusion matrix, highlighting correct and incorrect classifications with different colors.
  #'
  #' @param df A data frame containing the confusion matrix to be plotted. The rows represent the actual labels, and the columns represent the predicted labels.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @return A Plotly plot object displaying the confusion matrix.
  #' @export
  #' @importFrom plotly plot_ly layout
  #' @examples
  #' conf_matrix <- table(Predicted = c('A', 'B', 'A', 'A', 'B', 'B'), Actual = c('A', 'B', 'B', 'A', 'A', 'B'))
  #' plot_confusion_matrix(data.frame(table = conf_matrix))
  plot_confusion_matrix <- function(df, coord_fixed = TRUE) {
    #Check if input data frame is empty and return a message if so
    if (is.null(df$table)) {
      return(plot_message(message = "No model to evaluate"))
    }
    
    # Extract counts from confusion matrix
    conf_matrix <- as.matrix(df$table)
    x_labels <- colnames(conf_matrix)
    y_labels <- rownames(conf_matrix)
    
    # Create a matrix indicating correct (1) or incorrect (0) classifications
    correct <- matrix(0, nrow = nrow(conf_matrix), ncol = ncol(conf_matrix))
    for (i in 1:nrow(conf_matrix)) {
      for (j in 1:ncol(conf_matrix)) {
        if (rownames(conf_matrix)[i] == colnames(conf_matrix)[j]) {
          correct[i, j] <- 1 # Correct classification
        } else {
          correct[i, j] <- 0 # Incorrect classification
        }
      }
    }
    
    # Set fill colors for main plot
    fill_main <- list(
      list(0, colorspace::lighten(red_color, amount = 0.6)),
      list(1, colorspace::lighten(green_color, amount = 0.6))
    )
    
    # Set fill colors for background plot
    fill_background <- list(
      list(0, colorspace::lighten(red_color, amount = 0.3)),
      list(1, colorspace::lighten(green_color, amount = 0.3))
    )
    
    # Set color for text
    text_correct = colorspace::lighten(green_color, amount = -0.3)
    text_incorrect = colorspace::lighten(red_color, amount = -0.3)
    
    # Make plot
    plot <- plotly::plot_ly() %>%
      #Add background (provides border)
      plotly::add_trace(
        type = 'heatmap',
        x = x_labels,
        y = y_labels,
        z = correct,
        colorscale = fill_background,
        xgap = 5,
        ygap = 5,
        showscale = FALSE,
        text = conf_matrix,
        texttemplate = "%{text}",
        textfont = list(color = 'black', size = 16)
      ) %>%
      #Add main plot
      plotly::add_trace(
        type = 'heatmap',
        x = x_labels,
        y = y_labels,
        z = correct,
        colorscale = fill_main,
        xgap = 8,
        ygap = 8,
        showscale = FALSE
      )
    
    # Add annotations for each cell with different colors
    annotations <- list()
    for (i in 1:nrow(conf_matrix)) {
      for (j in 1:ncol(conf_matrix)) {
        color <- ifelse(correct[i, j] == 1, text_correct, text_incorrect)
        annotations <- append(annotations, list(
          list(
            x = x_labels[j],
            y = y_labels[i],
            text = conf_matrix[i, j],
            showarrow = FALSE,
            font = list(
              color = color,
              size = 16
            )
          )
        ))
      }
    }
    
    # Apply dynamic axis settings using generate_axis_settings
    axis_settings <- generate_axis_settings(
      coord_fixed = coord_fixed,   
      x_to_y_ratio = 1,
      showticklabels.x = TRUE,
      showticklabels.y = TRUE,
      ticklen.x = 4,
      ticklen.y = 4,
      title.x = "Predicted",
      title.y = "Actual",
      side.x = 'top',
      autorange.y = 'reversed',
      tickangle.y = -90,
      type = "category",   
      categoryorder = "array", 
      categoryarray.x = x_labels,
      categoryarray.y = y_labels
    )
    
    # Use the updated settings in the plot
    plot <- plot %>% plotly::layout(
      annotations = annotations,
      xaxis = c(axis_settings$xaxis),
      yaxis = c(axis_settings$yaxis),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
    
    return(plot)
  }

  #' Plot Metrics Table
  #'
  #' This function generates a Plotly table to display key evaluation metrics for 
  #' model performance, including accuracy, balanced accuracy, sensitivity, 
  #' specificity, precision, and F1 score.The values are color-coded using a 
  #' gradient, with 0 in red and 1 in green, to provide visual feedback on the model's performance.
  #'
  #' @param df A confusion matrix object containing overall and byClass metrics.
  #' @param header_color The background color to use for the table header.
  #' @param header_color The background color to use for the table cells.
  #' @param low_color The color to use for low values in the gradient.
  #' @param high_color The color to use for high values in the gradient.
  #' @return A Plotly table object displaying the metrics.
  #' @export
  #' @importFrom plotly plot_ly
  #' @importFrom scales rescale
  #' @importFrom colorspace lighten
  plot_metrics_table <- function(df, header_color = "#E9F0FF", cell_color = "#FDFDFF", low_color = "#FF0000", high_color = "#00B050")
  {
    #Check if input data frame is empty NULL if so
    if(is.null(df$overall) | is.null(df$byClass)) 
    {
      return(NULL)
    }
    
    # Extract the relevant metrics from df
    metrics <- data.frame(
      Metric = c("Accuracy", "Balanced Accuracy", "Sensitivity", "Specificity", "Precision", "F1 Score"),
      Value = c(
        df$overall["Accuracy"],
        df$byClass["Balanced Accuracy"],
        df$byClass["Sensitivity"],
        df$byClass["Specificity"],
        df$byClass["Precision"],
        df$byClass["F1"]
      )
    )
    
    # Convert the metrics to a data frame for use in plotly
    metrics <- as.data.frame(metrics)
    
    # Round values to two decimal places and ensure proper formatting
    metrics$Value <- as.numeric(metrics$Value)
    metrics$Value <- round(metrics$Value, 2)
    
    # Create a color gradient from red to green based on the values
    color_palette <- colorRampPalette(c(low_color, high_color))
    colors <- color_palette(101) # Create a gradient with 101 colors for values from 0 to 1
    value_colors <- colors[round(metrics$Value * 100) + 1] # Map values to colors
    
    # Format the values to two decimal places for display
    formatted_values <- format(metrics$Value, nsmall = 2)
    
    # Create the plotly table
    plot <- plotly::plot_ly(
      type = 'table',
      header = list(
        values = c("<b>Metric</b>", "<b>Value</b>"),
        align = c('center', 'center'),
        line = list(width = 1, color = 'white'),
        fill = list(color = header_color),
        font = list(size = 12, color = 'black')
      ),
      cells = list(
        values = rbind(metrics$Metric, formatted_values),
        align = c('center', 'center'),
        line = list(color = 'white', width = 1),
        fill = list(color = cell_color),
        font = list(size = 12, color = list(c('black'), value_colors))
      )
    )
    
    # Display the table
    return(plot)
  }
  
# --- Overlay plots ---
  #' Overlay Multiple Plotly Objects
  #' 
  #' This function overlays multiple Plotly plots, combining their traces and layout settings.
  #' 
  #' @param ... Multiple Plotly plot objects to be overlaid.
  #' @return A Plotly plot object combining the traces and layout settings of all input plots.
  #' @export
  #' @importFrom plotly plotly_build add_trace layout
  overlay_plots <- function(...) {
    plots <- list(...)
    
    if (length(plots) < 2) {
      stop("At least two plots are required for overlaying.")
    }
    
    # Start with the first plot
    combined_plot <- plots[[1]]
    
    for (i in 2:length(plots)) {
      plot_to_add <- plots[[i]]
      
      # Extract the traces from the next plot
      plot_traces <- plotly::plotly_build(plot_to_add)$x$data
      
      # Extract layout elements from both plots
      plot_layout <- plotly::plotly_build(plot_to_add)$x$layout
      
      # Add each trace from plot_traces to the combined plot
      for (trace in plot_traces) {
        combined_plot <- combined_plot %>% plotly::add_trace(
          x = trace$x,
          y = trace$y,
          mode = trace$mode %||% "markers",
          type = trace$type %||% "scatter",
          marker = trace$marker,
          line = trace$line,
          text = trace$text,
          hoverinfo = trace$hoverinfo,
          showlegend = FALSE 
        )
      }
      
      # Merge layout settings
      combined_plot <- combined_plot %>% plotly::layout(
        shapes = c(plotly::plotly_build(combined_plot)$x$layout$shapes, plot_layout$shapes),
        annotations = c(plotly::plotly_build(combined_plot)$x$layout$annotations[[1]], plot_layout$annotations[[1]])
      )
    }
    
    return(combined_plot)
  }
  
# --- Format results for plotting ---
  #' Format Prediction Results for Plots
  #'
  #' This function formats the results from app predictions into a format suitable 
  #' for different types of plots such as summary, heatmap, or treemap.
  #'
  #' @param df A dataframe containing the machine learning prediction results.
  #' @param plot_type A character string specifying the type of plot ("summary", "heatmap", or "treemap").
  #' @param x_col A character string specifying the column name to use as the x-axis (default: "Organism number").
  #' @param y_col A character string specifying the column name to use as the y-axis (default: "Trait name").
  #' @param z_col A character string specifying the column name representing values (default: "Probability").
  #' @param var_col Optional. A character string specifying the variable column name.
  #' @param var_to_keep Optional. A value to filter the dataframe by a specific value in `var_col`.
  #' @param z_threshold A numeric value specifying the threshold below which `z` values will be set to 0 (default: 0).
  #' @param drop_extra_y A logical indicating whether to remove `y` groups where all `z` values are 0 (default: TRUE).
  #' @param z_percentage A logical indicating whether to convert `z` values to percentages (default: TRUE).
  #' 
  #' @return A formatted dataframe ready for plotting.
  #' @export
  #' @importFrom dplyr rename mutate group_by filter ungroup arrange select summarize n
  #' @importFrom tidyr pivot_wider
  #' @importFrom rlang sym
  results_to_plot <- function(df, plot_type,
                              x_col = "Organism number", y_col = "Trait name", z_col = "Probability", 
                              var_col = NULL, var_to_keep = NULL, 
                              z_threshold = 0, drop_extra_y = TRUE, z_percentage = TRUE) 
  {
    # Rename columns
    if(!is.null(var_col))
    {
      df <- df %>% dplyr::rename(
        x = !!rlang::sym(x_col), 
        y = !!rlang::sym(y_col), 
        z = !!rlang::sym(z_col),
        var = !!rlang::sym(var_col),
      )
    }else{
      df <- df %>% dplyr::rename(
        x = !!rlang::sym(x_col), 
        y = !!rlang::sym(y_col), 
        z = !!rlang::sym(z_col)
      )
    }
    
    # Identify values of z below threshold and replace with 0
    df <- df %>% dplyr::mutate(z = ifelse(z >= z_threshold, z, 0))
    
    # Remove groups of y where all values of z are 0
    if(drop_extra_y)
    {
      if(!is.null(var_col))
      {
        df <- df %>%
          dplyr::group_by(var, y) %>%                     
          dplyr::filter(!(all(z == 0))) %>%           
          dplyr::ungroup() 
      }else{
        df <- df %>%
          dplyr::group_by(y) %>%                     
          dplyr::filter(!(all(z == 0))) %>%           
          dplyr::ungroup() 
      }
    }
    
    # Filter by var
    if(!is.null(var_col)&!is.null(var_to_keep))
    {
      df <- df %>%
        dplyr::filter(var == var_to_keep) %>%
        dplyr::select(-var)
    }
    
    # Ensure y is in alphabetical order
    df <- df %>% 
      dplyr::arrange(tolower(y)) %>%
      dplyr::mutate(y = factor(y, levels = unique(y)))
    
    # Remove extra columns
    df <- df %>% dplyr::select(x, y, z)
    
    if(nrow(df)==0)
    {
      return(df)
    }else if(plot_type == "summary") {
      # Further formatting
      df = df %>%
        dplyr::mutate(z = ifelse(z > 0, 100, 0))
      
      # Summarize
      df = df %>%
        dplyr::group_by(y) %>%
        dplyr::summarize(z = mean(z), .groups = 'drop') %>%
        dplyr::ungroup()
      
      # Pivot wider
      df <- df %>%
        tidyr::pivot_wider(names_from = y, values_from = z, values_fill = list(z = 0))
    }else if (plot_type == "heatmap") {
      # Convert to percentage
      if(z_percentage)
      {
        df$z <- df$z *100
      }
      
      # Factorize by x
      df$x <- factor(df$x, levels = unique(df$x))
      
      # Pivot wider
      df <- df %>%
        tidyr::pivot_wider(names_from = y, values_from = z)
      
    }else if (plot_type == "treemap") {
      # Further formatting
      df = df %>%
        dplyr::mutate(z = ifelse(z>0, 100, 0))
      
      # Summarize
      df <- df %>%
        dplyr::group_by(y) %>%
        dplyr::summarize(z = mean(z), .groups = "drop") 
      
      # Normalize to percentage
      df <- df %>%
        dplyr::mutate(z = (z / sum(z)) * 100)
    }
    
    return(df)
  }
  
# --- Other ---  
  #' Get Plotly Output Dimensions
  #'
  #' Retrieves the height and width of a Plotly plot from `session$clientData` in an RShiny session.
  #'
  #' @param session The Shiny session object.
  #' @param ns The namespace function for the module.
  #' @param plot_id The ID of the plot output.
  #' @param default_height Default height if clientData is NULL (default: 500).
  #' @param default_width Default width if clientData is NULL (default: 500).
  #'
  #' @return A named list containing `height` and `width` of the plot.
  #'
  #' @examples
  #' get_plotly_dimensions(session, ns, "heatmap_plot")
  #' get_plotly_dimensions(session, ns, "treemap_plot", default_height = 600, default_width = 800)
  get_plotly_dimensions <- function(session = getDefaultReactiveDomain(), 
                                    ns, plot_id, default_height = 500, default_width = 500) {
    height_key <- paste0("output_", ns(plot_id), "_height")
    width_key <- paste0("output_", ns(plot_id), "_width")
    
    plot_height <- session$clientData[[height_key]] %||% default_height
    plot_width <- session$clientData[[width_key]] %||% default_width
    
    return(list(height = plot_height, width = plot_width))
  }
  
  #' Calculate Dimensions for Treemap Plot
  #'
  #' Retrieves the height of a treemap plot from `session$clientData` and computes the width.
  #'
  #' @param session The Shiny session object.
  #' @param ns The namespace function for the module.
  #' @param plot_id The ID of the treemap plot output.
  #' @param scaling_factor The factor used to determine width based on height (default: 1.045296).
  #' @param default_height Default height if clientData is NULL (default: 500).
  #'
  #' @return A named list containing `height` and `width` for the treemap plot.
  #'
  #' @examples
  #' treemap_dims <- calculate_treemap_dimensions(session, ns, "treemap_plot")
  #' plot_treemap2(df, width = treemap_dims$width, height = treemap_dims$height)
  calculate_treemap_dimensions <- function(session = getDefaultReactiveDomain(), 
                                           ns, plot_id, scaling_factor = 1.045296, default_height = 500) {
    dims <- get_plotly_dimensions(session, ns, plot_id, default_height = default_height)
    list(
      height = dims$height,
      width = dims$height * scaling_factor
    )
  }
  
  #' Calculate Scale Ratio for Plotly Heatmap
  #'
  #' Computes the aspect ratio scaling factor for a heatmap based on the display dimensions
  #' and the data dimensions.
  #'
  #' @param session The Shiny session object.
  #' @param ns The namespace function for the module.
  #' @param plot_id The ID of the Plotly output.
  #' @param df The dataframe used in the plot.
  #'
  #' @return A numeric value representing the `scale_ratio`, ensuring proper aspect ratio.
  #'
  #' @examples
  #' scale_ratio <- calculate_scale_ratio(session, ns, "heatmap_plot", df)
  calculate_heatmap_scale <- function(session = getDefaultReactiveDomain(), 
                                      ns, plot_id, df) {
    dims <- get_plotly_dimensions(session, ns, plot_id)
    display_ratio <- dims$height / dims$width
    data_ratio <- nrow(df) / ncol(df)
    scale_ratio <- data_ratio / display_ratio
    max(1, scale_ratio)
  }