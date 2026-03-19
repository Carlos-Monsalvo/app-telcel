library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(treemapify)
library(packcircles)
library(ggforce)
library(prophet)



install.packages("waffle")

set.seed(123)

# ------------------ Datos simulados ------------------
n <- 1500

data <- data.frame(
  fecha = seq.Date(Sys.Date()-1499, Sys.Date(), by="day")
)

data <- data %>%
  mutate(
    año = year(fecha),
    mes = month(fecha),
    dia_semana = weekdays(fecha),
    
    idCliente = sample(10000:99999, n, replace = TRUE),
    edad = sample(18:70, n, replace = TRUE),
    
    genero = sample(c("Masculino","Femenino"), n, replace = TRUE),
    
    tipoCliente = sample(c("Recargas","Planes"), n, replace = TRUE, prob = c(0.6,0.4)),
    antiguedad_meses = sample(1:60, n, replace = TRUE),
    
    tipoProducto = sample(c("Chip","Plan","Equipo"), n, replace = TRUE, prob = c(0.4,0.3,0.3)),
    
    marcaEquipo = sample(c("Samsung","Apple","Xiaomi","Motorola","NA"), n, replace = TRUE),
    
    modeloEquipo = paste("Modelo", sample(1:20, n, replace = TRUE)),
    
    gama = sample(c("Baja","Media","Alta"), n, replace = TRUE, prob = c(0.4,0.4,0.2)),
    
    ventas = round(runif(n, 100, 20000)),
    costo = ventas * runif(n, 0.5, 0.8),
    margen = ventas - costo,
    
    metodoPago = sample(c("Efectivo","Tarjeta","Transferencia"), n, replace = TRUE),
    
    vendedor = sample(paste("Vendedor", 1:10), n, replace = TRUE),
    
    sucursal = sample(c("Norte","Centro","Sur","Oriente","Poniente"), n, replace = TRUE),
    
    numeroCompras = sample(1:10, n, replace = TRUE),
    frecuenciaCompra = round(runif(n, 0.1, 3),2),
    
    descuento = round(runif(n, 0, 0.3),2)
  )

# KPI derivados
data <- data %>%
  mutate(
    GastoPromedio = ventas / numeroCompras,
    ROI = (ventas - costo) / costo
  )

# Segmentación de edad
data <- data %>%
  mutate(
    rango_edad = case_when(
      edad >= 18 & edad <= 25 ~ "18-25",
      edad >= 26 & edad <= 35 ~ "26-35",
      edad >= 36 & edad <= 45 ~ "36-45",
      edad >= 46 & edad <= 55 ~ "46-55",
      edad >= 56 ~ "56+"
    )
  )

meses <- c("enero","febrero","marzo","abril","mayo","junio",
           "julio","agosto","septiembre","octubre","noviembre","diciembre")

data <- data %>%
  mutate(mes_nombre = meses[mes])

# ------------------ UI ------------------
ui <- dashboardPage(
  
  dashboardHeader(title = "Dashboard Telcel"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("General", tabName = "general", icon = icon("chart-pie")),
      menuItem("KPI", tabName = "kpi", icon = icon("tachometer-alt")),
      menuItem("Series de tiempo", tabName = "series", icon = icon("chart-line"))
      
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ---------------- GENERAL ----------------
      tabItem(tabName = "general",
              fluidRow(
                box(plotOutput("ventasMesPlot"), width = 6),
                box(plotOutput("ventasAnioPlot"), width = 6)
              ),
              fluidRow(
                box(plotOutput("generoPlot"), width = 4),
                box(plotOutput("edadPlot"), width = 4),
                box(plotOutput("clientePlot"), width = 4)
              ),
              fluidRow(
                box(plotOutput("productoPlot"), width = 4),
                box(plotOutput("vendedorPlot"), width = 4),
                box(plotOutput("sucursalPlot"), width = 4)
              )
      ),
      
      # ---------------- KPI ----------------
      tabItem(tabName = "kpi",
              fluidRow(
                valueBoxOutput("gastoPromedioBox", width = 3),
                valueBoxOutput("margenBox", width = 3),
                valueBoxOutput("roiBox", width = 3),
                valueBoxOutput("numComprasBox", width = 3)
              ),
              
              fluidRow(
                box(plotOutput("gastoPromedioPlot"), width = 6),
                box(plotOutput("margenPlot"), width = 6)
              ),
              
              fluidRow(
                box(plotOutput("ROIPlot"), width = 6),
                box(plotOutput("numeroComprasPlot"), width = 6)
              ),
              
              fluidRow(
                box(plotOutput("marcaEquipoPlot"), width = 6),
                box(plotOutput("metodoPagoPlot"), width = 6)
              )
      ),
      
      
      
      # ---------------- SERIES ----------------
      tabItem(tabName = "series",
              
              fluidRow(
                box(plotOutput("serieMensualPlot"), width = 12)
              ),
              
              fluidRow(
                box(plotOutput("forecastPlot"), width = 12)
              ),
              
              fluidRow(
                box(tableOutput("tablaForecast"), width = 12)
              )
      )
    )
  )
)

# ------------------ SERVER ------------------
server <- function(input, output) {
  
  # ---------- ValueBoxes KPI ----------
  output$gastoPromedioBox <- renderValueBox({
    valueBox(
      value = scales::dollar(round(mean(data$GastoPromedio),2)),
      subtitle = "Gasto Promedio",
      icon = icon("credit-card"),
      color = "green"
    )
  })
  
  output$margenBox <- renderValueBox({
    valueBox(
      value = scales::dollar(round(mean(data$margen),2)),
      subtitle = "Margen Promedio",
      icon = icon("coins"),
      color = "yellow"
    )
  })
  
  output$roiBox <- renderValueBox({
    valueBox(
      value = paste0(round(mean(data$ROI)*100,2),"%"),
      subtitle = "ROI Promedio",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  output$numComprasBox <- renderValueBox({
    valueBox(
      value = round(mean(data$numeroCompras),2),
      subtitle = "Número Promedio de Compras",
      icon = icon("shopping-cart"),
      color = "blue"
    )
  })
  
  # ---------- Gráficos KPI ----------
  
  # Gasto Promedio por Vendedor
  output$gastoPromedioPlot <- renderPlot({
    
    data_plot <- data %>%
      group_by(vendedor) %>%
      summarise(GastoPromedio = mean(GastoPromedio), .groups = "drop") %>%
      arrange(GastoPromedio)
    
    ggplot(data_plot, aes(reorder(vendedor, GastoPromedio), GastoPromedio, fill = GastoPromedio)) +
      
      geom_col() +
      
      coord_flip() +
      
      # 🔥 etiquetas mejor posicionadas
      geom_text(
        aes(label = round(GastoPromedio, 1)),
        hjust = 1.2,         # dentro de la barra
        color = "white",
        size = 3             # más pequeñas
      ) +
      
      scale_fill_gradient(low = "#FFEC5C", high = "#146152") +
      
      labs(
        title = "Gasto Promedio por Vendedor",
        x = "",
        y = "Gasto Promedio"
      ) +
      
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 8)  # 🔹 nombres más compactos
      )
  })
  
  # Margen promedio por producto
  output$margenPlot <- renderPlot({
    
    library(dplyr)
    library(ggplot2)
    library(tidyr)
    
    # 1️⃣ Calcular proporciones
    data_plot <- data %>%
      group_by(tipoProducto) %>%
      summarise(MargenPromedio = mean(margen), .groups = "drop") %>%
      mutate(
        prop = MargenPromedio / sum(MargenPromedio),
        porcentaje = scales::percent(prop)
      )
    
    # 2️⃣ Crear bloques waffle
    waffle_data <- data_plot %>%
      mutate(n = round(prop * 100)) %>%
      uncount(n) %>%
      mutate(
        id = row_number(),
        x = (id - 1) %% 10,
        y = (id - 1) %/% 10
      )
    
    # 3️⃣ Calcular centro para etiquetas
    etiquetas <- waffle_data %>%
      group_by(tipoProducto) %>%
      summarise(
        x = mean(x),
        y = mean(y),
        .groups = "drop"
      ) %>%
      left_join(data_plot, by = "tipoProducto")
    
    # 4️⃣ Plot
    ggplot(waffle_data, aes(x, y, fill = tipoProducto)) +
      
      geom_tile(color = "white") +
      
      # 🔥 etiquetas de porcentaje
      geom_text(
        data = etiquetas,
        aes(x = x, y = y, label = porcentaje),
        color = "white",
        size = 5,
        fontface = "bold"
      ) +
      
      scale_fill_manual(values = c(
        "Chip" = "#146152",
        "Plan" = "#44803F",
        "Equipo" = "#B4CF66"
      )) +
      
      coord_equal() +
      
      labs(
        title = "Distribución del Margen por Producto",
        fill = "Producto"
      ) +
      
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
  })
  
  # ROI promedio por sucursal
  output$ROIPlot <- renderPlot({
    
    
    # 1️⃣ Calcular ROI promedio
    data_plot <- data %>%
      group_by(sucursal) %>%
      summarise(ROI_Promedio = mean(ROI), .groups = "drop") %>%
      arrange(desc(ROI_Promedio))
    
    # 2️⃣ Crear layout circular
    packing <- circleProgressiveLayout(data_plot$ROI_Promedio, sizetype = 'area')
    data_plot <- cbind(data_plot, packing)
    
    # 3️⃣ Generar vértices de los círculos
    vertices <- circleLayoutVertices(packing, npoints = 100)
    
    # 4️⃣ Plot
    ggplot() +
      
      geom_polygon(
        data = vertices,
        aes(x, y, group = id, fill = data_plot$ROI_Promedio[id]),
        color = "white"
      ) +
      
      geom_text(
        data = data_plot,
        aes(
          x = x,
          y = y,
          label = paste0(sucursal, "\n", round(ROI_Promedio, 2))
        ),
        color = "white",
        size = 4,
        fontface = "bold"
      ) +
      
      scale_fill_gradient(low = "#FFEC5C", high = "#146152") +
      
      coord_equal() +
      
      labs(title = "ROI Promedio por Sucursal") +
      
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "none"
      )
  })
  
  # Histograma de número de compras
  output$numeroComprasPlot <- renderPlot({
    
    # 1️⃣ Crear rangos
    data_plot <- data %>%
      mutate(rangoCompras = case_when(
        numeroCompras >= 1 & numeroCompras <= 3 ~ "1-3",
        numeroCompras >= 4 & numeroCompras <= 5 ~ "4-5",
        numeroCompras >= 6 ~ "6+"
      )) %>%
      count(rangoCompras) %>%
      arrange(desc(n)) %>%
      mutate(
        porcentaje = n / sum(n),
        etiqueta = paste0(rangoCompras, "\n", n, " (", scales::percent(porcentaje), ")"),
        id = row_number()
      )
    
    # 2️⃣ Ángulos
    data_plot <- data_plot %>%
      mutate(
        angulo = 90 - 360 * (id - 0.5) / n(),
        hjust = ifelse(angulo < -90, 1, 0),
        angulo = ifelse(angulo < -90, angulo + 180, angulo)
      )
    
    # 3️⃣ Gráfico
    ggplot(data_plot, aes(x = factor(id), y = n, fill = rangoCompras)) +
      
      geom_bar(stat = "identity", width = 1) +
      
      coord_polar(start = 0) +
      
      # 🔥 etiquetas con rango + valor + %
      geom_text(
        aes(
          label = etiqueta,
          y = n + max(n)*0.1,
          hjust = hjust
        ),
        angle = data_plot$angulo,
        size = 4
      ) +
      
      scale_fill_manual(values = c(
        "1-3" = "#146152",
        "4-5" = "#44803F",
        "6+"  = "#FF5A33"
      )) +
      
      labs(title = "Número de Compras ") +
      
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none"
      )
  })
  
  # Marca de equipo
  
  output$marcaEquipoPlot <- renderPlot({
    data_plot <- data %>%
      filter(marcaEquipo != "NA") %>%
      count(marcaEquipo) %>%
      mutate(porcentaje = n / sum(n))
    
    ggplot(data_plot, aes(
      area = n, 
      fill = marcaEquipo, 
      label = paste0(marcaEquipo, "\n", scales::percent(porcentaje))
    )) +
      geom_treemap() +
      geom_treemap_text(
        aes(label = paste0(marcaEquipo, "\n", scales::percent(porcentaje))),
        place = "centre",
        grow = TRUE,
        color = "white",
        fontface = "bold",
        size=5 #tamaño de letra
      ) +
      scale_fill_manual(values=c(
        "Samsung"="#146152",
        "Apple"="#FF5A33",
        "Xiaomi"="#B4CF66",
        "Motorola"="#44803F"
      )) +
      labs(title="Distribución por Marca de Equipo") +
      theme(legend.position = "none",
            plot.title = element_text(hjust=5.5, size=0.5))
  })
  
  # Método de pago
  output$metodoPagoPlot <- renderPlot({
    data_plot <- data %>%
      count(metodoPago) %>%
      mutate(porcentaje = n / sum(n))
    
    ggplot(data_plot, aes(
      area = n,
      fill = metodoPago,
      label = paste0(metodoPago, "\n", scales::percent(porcentaje))
    )) +
      geom_treemap() +
      geom_treemap_text(
        aes(label = paste0(metodoPago, "\n", scales::percent(porcentaje))),
        place = "centre",
        grow = TRUE,
        color = "white",
        fontface = "bold",
        size = 1  # 🔹 tamaño más pequeño
      ) +
      scale_fill_manual(values=c(
        "Efectivo" = "#146152",
        "Tarjeta" = "#FF5A33",
        "Transferencia" = "#B4CF66"
      )) +
      labs(title = "Distribución por Método de Pago") +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 1))
  })
  
  # ---------- Gráficos generales ----------
  # Ventas por mes
  output$ventasMesPlot <- renderPlot({
    data_plot <- data %>%
      group_by(mes, mes_nombre) %>%
      summarise(ventas = sum(ventas), .groups = "drop") %>%
      arrange(mes)
    
    ggplot(data_plot, aes(x = mes, y = ventas)) +
      geom_line(color = "#146152", linewidth = 1) +
      geom_point(color = "#FF5A33", size = 2) +
      geom_text(aes(label = scales::comma(ventas)), vjust = -0.8, size = 3.5) +
      scale_x_continuous(breaks = 1:12, labels = meses) +
      labs(title = "Ventas por Mes", x = "Mes", y = "Ventas") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      expand_limits(y = max(data_plot$ventas) * 1.1)
  })
  
  # Ventas por año
  output$ventasAnioPlot <- renderPlot({
    data_plot <- data %>%
      group_by(año) %>%
      summarise(ventas = sum(ventas), .groups = "drop") %>%
      arrange(año)
    
    ggplot(data_plot, aes(x = año, y = ventas)) +
      geom_line(color = "#146152", linewidth = 1.2) +
      geom_point(color = "#FF5A33", size = 3) +
      geom_text(aes(label = scales::comma(ventas)), vjust = -0.8, size = 4) +
      labs(title = "Ventas por Año", x = "Año", y = "Ventas") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      expand_limits(y = max(data_plot$ventas) * 1.1)
  })
  
  # Distribuciones generales (genero, edad, cliente, producto, vendedor, sucursal)
  
  output$generoPlot <- renderPlot({
    data_plot <- data %>%
      count(genero) %>%
      mutate(etiqueta = scales::percent(n/sum(n)))
    
    ggplot(data_plot, aes(x=2, y=n, fill=genero)) +
      geom_bar(stat="identity", width=1) +
      coord_polar(theta="y") +
      geom_text(aes(label=etiqueta), position=position_stack(vjust=0.5), color="white") +
      scale_fill_manual(values=c("Masculino"="#146152","Femenino"="#FF5A33")) +
      labs(title="Distribución por Género", fill="Género") +
      theme_void() +
      theme(plot.title = element_text(hjust=0.5))
  })
  
  output$edadPlot <- renderPlot({
    data_plot <- data %>%
      count(rango_edad) %>%
      mutate(etiqueta = scales::percent(n/sum(n)))
    
    ggplot(data_plot, aes(x=rango_edad, y=n, fill=rango_edad)) +
      geom_col() +
      geom_text(aes(label=n), vjust=-0.5) +
      geom_text(aes(label=etiqueta), vjust=1.5) +
      scale_fill_manual(values=c("18-25"="#146152","26-35"="#44803F","36-45"="#B4CF66",
                                 "46-55"="#FFEC5C","56+"="#FF5A33")) +
      labs(title="Distribución por Rango de Edad", x="Rango de edad", y="Frecuencia") +
      theme_minimal() +
      theme(plot.title=element_text(hjust=0.5))
  })
  
  output$clientePlot <- renderPlot({
    data_plot <- data %>%
      count(tipoCliente) %>%
      mutate(etiqueta = scales::percent(n/sum(n)))
    
    ggplot(data_plot, aes(tipoCliente, n, fill=tipoCliente)) +
      geom_col() +
      geom_text(aes(label=n), vjust=-0.5) +
      geom_text(aes(label=etiqueta), vjust=1.5) +
      scale_fill_manual(values=c("Recargas"="#FFEC5C","Planes"="#FF5A33")) +
      labs(title="Tipo de Cliente", x="Tipo de cliente", y="Frecuencia") +
      theme_minimal() +
      theme(plot.title=element_text(hjust=0.5), legend.position="none")
  })
  
  output$productoPlot <- renderPlot({
    data_plot <- data %>%
      count(tipoProducto) %>%
      mutate(etiqueta = scales::percent(n/sum(n)))
    
    ggplot(data_plot, aes(tipoProducto, n, fill=tipoProducto)) +
      geom_col() +
      geom_text(aes(label=n), vjust=-0.5) +
      geom_text(aes(label=etiqueta), vjust=1.5) +
      scale_fill_manual(values=c("Chip"="#146152","Plan"="#44803F","Equipo"="#B4CF66")) +
      labs(title="Tipo de Producto", x="Producto", y="Cantidad") +
      theme_minimal() +
      theme(plot.title=element_text(hjust=0.5), legend.position="none")
  })
  
  output$vendedorPlot <- renderPlot({
    data_plot <- data %>%
      count(vendedor) %>%
      arrange(n) %>%
      mutate(orden=row_number())
    
    ggplot(data_plot, aes(reorder(vendedor,n), n, fill=orden)) +
      geom_col() +
      coord_flip() +
      geom_text(aes(label=n), hjust=-0.2) +
      scale_fill_gradientn(colors=c("#146152","#44803F","#B4CF66","#FFEC5C","#FF5A33")) +
      labs(title="Ventas por Vendedor") +
      theme_minimal() +
      theme(plot.title=element_text(hjust=0.5), legend.position="none")
  })
  
  output$sucursalPlot <- renderPlot({
    data_plot <- data %>%
      count(sucursal) %>%
      mutate(etiqueta = scales::percent(n/sum(n)))
    
    ggplot(data_plot, aes(sucursal, n, fill=sucursal)) +
      geom_col() +
      geom_text(aes(label=n), vjust=-0.5) +
      labs(title="Distribución por Sucursal", x="Sucursal", y="Frecuencia") +
      scale_fill_manual(values=colorRampPalette(c("#146152","#44803F","#B4CF66","#FFEC5C","#FF5A33"))(nrow(data_plot))) +
      theme_minimal() +
      theme(plot.title=element_text(hjust=0.5), legend.position="none")
  })
  
  
  # ---------------- SERIES ----------------
  
  data_ts <- reactive({
    data %>%
      mutate(fecha_mes = floor_date(fecha, "month")) %>%
      group_by(fecha_mes) %>%
      summarise(ventas = sum(ventas), .groups = "drop")
  })
  
  output$serieMensualPlot <- renderPlot({
    df <- data_ts()
    
    ggplot(df, aes(fecha_mes, ventas)) +
      geom_line(color = "#146152") +
      geom_point(color = "#FF5A33") +
      theme_minimal() +
      labs(title = "Serie Mensual")
  })
  
  forecast_data <- reactive({
    
    df <- data_ts() %>%
      rename(ds = fecha_mes, y = ventas)
    
    df$ds <- as.Date(df$ds)
    
    modelo <- prophet(df)
    
    futuro <- make_future_dataframe(modelo, periods = 12, freq = "month")
    
    forecast <- predict(modelo, futuro)
    forecast$ds <- as.Date(forecast$ds)
    
    list(df = df, forecast = forecast)
  })
  
  output$forecastPlot <- renderPlot({
    
    res <- forecast_data()
    df <- res$df
    forecast <- res$forecast
    
    df_join <- df %>%
      left_join(forecast, by = "ds") %>%
      mutate(
        anomaly = ifelse(y > yhat_lower | y > yhat_upper, "Anomalía", "Normal")
      )
    
    corte <- max(df$ds)
    
    ggplot() +
      geom_line(data = df, aes(ds, y), color = "#146152") +
      geom_line(data = forecast, aes(ds, yhat), color = "#FF5A33", linetype = "dashed") +
      geom_ribbon(data = forecast,
                  aes(ds, ymin = yhat_lower, ymax = yhat_upper),
                  fill = "#FF5A33", alpha = 0.2) +
      geom_point(
        data = df_join %>% filter(anomaly == "Anomalía"),
        aes(ds, y),
        color = "red",
        size = 3
      ) +
      geom_vline(xintercept = corte, linetype = "dotted") +
      theme_minimal() +
      labs(title = "Forecast")
  })
  
  output$tablaForecast <- renderTable({
    
    res <- forecast_data()
    forecast <- res$forecast
    
    #  Asegurar formato Date
    forecast$ds <- as.Date(forecast$ds)
    res$df$ds <- as.Date(res$df$ds)
    
    corte <- max(res$df$ds, na.rm = TRUE)
    
    forecast %>%
      filter(ds > corte) %>%
      select(
        Fecha = ds,
        Prediccion = yhat,
        Min = yhat_lower,
        Max = yhat_upper
      ) %>%
      mutate(
        Fecha = format(Fecha, "%Y-%m-%d"),
        across(-Fecha, round, 0)
      )
  })
  
  
}

# ------------------ Launch App ------------------
shinyApp(ui, server)