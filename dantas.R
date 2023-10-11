cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")
theme_estat <- function(...) {
    theme <- ggplot2::theme_bw() +
        ggplot2::theme(
            axis.title.y = ggplot2::element_text(colour = "black", size = 12),
            axis.title.x = ggplot2::element_text(colour = "black", size = 12),
            axis.text = ggplot2::element_text(colour = "black", size = 9.5),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            legend.position = "top",
            ...
        )

    return(
        list(
            theme,
            scale_fill_manual(values = cores_estat),
            scale_colour_manual(values = cores_estat)
        )
    )
}

#arrumando os bancos#
vendas <- rename(vendas, Motivo.Devolucao = Motivo.devolução)
vendas <- rename(vendas, ID.Produto = Product.ID)
vendas <- rename(vendas, ID.Usuario = User.ID)
vendas <- rename(vendas, Nome.Produto = Product.Name)
vendas <- rename(vendas, Marca = Brand)
vendas <- rename(vendas, Categoria = Category)
vendas <- rename(vendas, Avaliacao = Rating)
vendas <- rename(vendas, Cor = Color)
vendas <- rename(vendas, Preco = Price)
vendas <- rename(vendas, Tamanho = Size)
vendas <- rename(vendas, ID.Unico = Unique.ID)
devolucao <- rename(devolucao, ID.Unico = Unique.ID)
devolucao <- rename(devolucao, Motivo.Devolucao = Motivo.devolução)

vendas$Nome.Produto[vendas$Nome.Produto == "T-shirt"] <- "Camisa"
vendas$Nome.Produto[vendas$Nome.Produto == "Jeans"] <- "Calça Jeans"
vendas$Nome.Produto[vendas$Nome.Produto == "Dress"] <- "Vestido"
vendas$Nome.Produto[vendas$Nome.Produto == "Shoes"] <- "Sapato"
vendas$Nome.Produto[vendas$Nome.Produto == "Sweater"] <- "Casaco"
vendas$Cor[vendas$Cor == "Blue"] <- "Azul"
vendas$Cor[vendas$Cor == "Red"] <- "Vermelho"
vendas$Cor[vendas$Cor == "Black"] <- "Preto"
vendas$Cor[vendas$Cor == "Yellow"] <- "Amarelo"
vendas$Cor[vendas$Cor == "White"] <- "Branco"
vendas$Cor[vendas$Cor == "Green"] <- "Verde"
vendas$Tamanho[vendas$Tamanho == "L"] <- "G"
vendas$Tamanho[vendas$Tamanho == "XL"] <- "GG"
vendas$Tamanho[vendas$Tamanho == "S"] <- "P"
vendas$Categoria[vendas$Categoria == "Women's Fashion"] <- "Moda Feminina"
vendas$Categoria[vendas$Categoria == "Men's Fashion"] <- "Moda Masculina"
vendas$Categoria[vendas$Categoria == "Kids' Fashion"] <- "Moda Infantil"

vendas <- vendas %>%
  mutate(Nome.Produto = ifelse(is.na(Nome.Produto), "Desconhecido", Nome.Produto))
vendas <- vendas %>%
  mutate(Motivo.Devolucao = ifelse(is.na(Motivo.Devolucao), "Não Devolvido", Motivo.Devolucao))
vendas <- vendas %>%
  mutate(Categoria = ifelse(is.na(Categoria), "Desconhecida", Categoria))
vendas <- vendas %>%
  mutate(Data.Venda = ifelse(is.na(Data.Venda), "Desconhecida", Data.Venda))
vendas <- vendas %>%
  mutate(Marca = ifelse(is.na(Marca), "Desconhecida", Marca))
vendas <- vendas %>%
  mutate(Cor = ifelse(is.na(Cor), "Desconhecida", Cor))
vendas <- vendas %>%
  mutate(Tamanho = ifelse(is.na(Tamanho), "Desconhecido", Tamanho))

vendas <- subset(vendas, !is.na(Preco))
