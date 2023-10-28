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

#ARRUMANDO OS BANCOS#
library(tidyverse)

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
  mutate(Motivo.Devolucao = ifelse(is.na(Motivo.Devolucao), "Não Devolvido", Motivo.Devolucao))

vendas <- subset(vendas, !is.na(Preco))
vendas <- subset(vendas, !is.na(Marca))
vendas <- subset(vendas, !is.na(Nome.Produto))
vendas <- subset(vendas, !is.na(Categoria))
vendas <- subset(vendas, !is.na(Data.Venda))
vendas <- subset(vendas, !is.na(Cor))
vendas <- subset(vendas, !is.na(Tamanho))
vendas <- subset(vendas, !is.na(ID.Usuario))
vendas <- subset(vendas, !is.na(ID.Produto))
vendas <- subset(vendas, !is.na(Avaliacao))

##ANALISE 1##
#ORGANIZANDO AS CATEGORIAS E O FATURAMENTO#

vendas_sem_devolucao <- vendas %>%
  filter(Motivo.Devolucao == "Não Devolvido")
vendas_devolvidas <- vendas %>%
  filter(Motivo.Devolucao != "Não Devolvido")

vendas_faturamento_perdido_categoria <- vendas_devolvidas %>% 
  group_by(Categoria) %>% 
  summarise(Faturamento = sum(Preco))
vendas_faturamento_categoria <- vendas %>% 
  group_by(Categoria) %>% 
  summarise(Faturamento = sum(Preco))

vendas_faturamento_perdido_total <- vendas_faturamento_perdido_categoria %>% 
  summarise(Faturamento.Total = sum(Faturamento))
vendas_faturamento_total <- vendas_faturamento_categoria %>% 
  summarise(Faturamento.Total = sum(Faturamento))

#GRAFICOS#

ggplot(vendas_faturamento_categoria) +
  aes(x = fct_reorder(Categoria, Faturamento, .desc=T), y = Faturamento, label = Faturamento) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Categoria", y = "Faturamento") +
  theme_estat()
ggsave("colunas_categoria_faturamento.pdf", width = 158, height = 93, units = "mm")

##ANALISE 2##

vendas$Marca <- factor(vendas$Marca)

by(vendas$Preco, vendas$Marca, quantile)
by(vendas$Preco, vendas$Marca, mean)

min <- c(10,10,10,10,10)
quartil1 <- c(41,37,38,38.5,44)
mediana <- c(52,50,49,50,54)
quartil3 <- c(62,61,59,61,63.5)
max <- c(96,92,100,90,90)
media <- c(51.75587,49.69849,49.40306,49.60352,53.39409)

sd(min)
sd(quartil1)
sd(mediana)
sd(quartil3)
sd(max)
sd(media)

#GRAFICOS#

ggplot(vendas) +
  aes(x = Marca, y = Preco) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marca", y = "Preço") +
  theme_estat()
ggsave("box_marcapreco.pdf", width = 158, height = 93, units = "mm")

##ANALISE 3##

#relação entre categorias (feminino e masculino e cor)#

vendas_masc_fem <- vendas %>% 
  filter(Categoria != "Moda Infantil")

#GRAFICOS#

cores_desejadas <- c("Azul" = "#2986cc", "Vermelho" = "#f44336", "Preto" = "#444444", "Branco" = "#d5d5d5", "Amarelo" = "#e4de07", "Verde" = "#78d51c")

categoria_cor <- vendas_masc_fem %>%
  mutate(Categoria = case_when(
    Categoria %>% str_detect("Moda Feminina") ~ "Moda Feminina",
    Categoria %>% str_detect("Moda Masculina") ~ "Moda Masculina"
  )) %>%
  group_by(Categoria, Cor) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = scales::percent(freq / sum(freq))
  )

porcentagens <- str_c(categoria_cor$freq_relativa) %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(categoria_cor$freq, " (", porcentagens, ")"))

ggplot(categoria_cor) +
  aes(
    x = fct_reorder(Categoria, freq, .desc = TRUE), y = freq, fill = Cor, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Categoria", y = "Cor") +
  theme_estat() +
  scale_fill_manual(values = cores_desejadas)
ggsave("colunas_cat_cor.pdf", width = 158, height = 93, units = "mm")

dados_resumidos <- vendas_masc_fem %>%
  group_by(Categoria, Cor) %>%
  summarise(Frequencia = n())

ggplot(dados_resumidos, aes(x = Categoria, y = Frequencia, fill = Cor)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Categoria", y = "Frequência", fill = "Cor") +
  scale_fill_manual(values = cores_desejadas)

