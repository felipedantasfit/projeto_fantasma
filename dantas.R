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

vendas <- read.csv("vendas.csv")
devolucao <- read.csv("devolução.csv")

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

#vendas <- na.omit(vendas)
#vendas <- subset(vendas, !is.na(Preco))
#vendas <- subset(vendas, !is.na(Marca))
#vendas <- subset(vendas, !is.na(Nome.Produto))
#vendas <- subset(vendas, !is.na(Categoria))
#vendas <- subset(vendas, !is.na(Data.Venda))
#vendas <- subset(vendas, !is.na(Cor))
#vendas <- subset(vendas, !is.na(Tamanho))
#vendas <- subset(vendas, !is.na(ID.Usuario))
#vendas <- subset(vendas, !is.na(ID.Produto))
#vendas <- subset(vendas, !is.na(Avaliacao))

vendas <- vendas %>% 
  distinct(ID.Produto, .keep_all = TRUE)

vendas <- vendas[, !names(vendas) %in% "Motivo.Devolucao"]
vendas <- vendas %>%
  left_join(devolucao, by = "ID.Unico")
vendas$...1.y <- NULL
vendas$...1.x <- NULL
vendas$X.y <- NULL
vendas$X.x <- NULL

vendas <- vendas %>%
  mutate(Motivo.Devolucao = ifelse(is.na(Motivo.Devolucao), "Não Devolvido", Motivo.Devolucao))

##ANALISE 1##
#ORGANIZANDO AS CATEGORIAS E O FATURAMENTO#

library(lubridate)

vendas$Data.Venda <- mdy(vendas$Data.Venda)
vendas$Mes <- month(vendas$Data.Venda)
vendas$Mes <- as.character(vendas$Mes)

vendas$Mes[vendas$Mes == "1"] <- "Jan"
vendas$Mes[vendas$Mes == "2"] <- "Fev"
vendas$Mes[vendas$Mes == "3"] <- "Mar"
vendas$Mes[vendas$Mes == "4"] <- "Abr"
vendas$Mes[vendas$Mes == "5"] <- "Mai"
vendas$Mes[vendas$Mes == "6"] <- "Jun"
vendas$Mes[vendas$Mes == "7"] <- "Jul"
vendas$Mes[vendas$Mes == "8"] <- "Ago"
vendas$Mes[vendas$Mes == "9"] <- "Set"
vendas$Mes[vendas$Mes == "10"] <- "Out"
vendas$Mes[vendas$Mes == "11"] <- "Nov"
vendas$Mes[vendas$Mes == "12"] <- "Dez"

vendas_fatcat <- subset(vendas, !is.na(Data.Venda))
vendas_fatcat <- subset(vendas_fatcat, !is.na(Preco))
vendas_fatcat <- subset(vendas_fatcat, !is.na(Categoria))


vendas_fatcat <- vendas_fatcat %>% 
  group_by(Categoria, Mes) %>%
  summarise(Faturamento = sum(Preco))

vendas_fatcat$Mes <- factor(vendas_fatcat$Mes, levels = c(
  "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
  "Jul", "Ago", "Set", "Out", "Nov", "Dez"
))

#GRAFICOS#

ggplot(vendas_fatcat) +
  aes(x = Mes, y = Faturamento, group = Categoria, colour = Categoria) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Categoria", labels = c("Moda Feminina", "Moda Infantil", "Moda Masculina")) +
  labs(x = "Mês", y = "Faturamento") +
  theme_estat()
ggsave("Linha_fatcat.pdf", width = 158, height = 93, units = "mm")

##ANALISE 2##

vendas$Marca <- factor(vendas$Marca)

vendas_prec_marc <- subset(vendas, !is.na(Preco))
vendas_prec_marc <- subset(vendas_prec_marc, !is.na(Marca))

quadro_resumo <- vendas_prec_marc %>% 
  group_by(Marca) %>% # caso mais de uma categoria
  summarize(Média = round(mean(Preco),2),
            `Desvio Padrão` = round(sd(Preco),2),
            `Variância` = round(var(Preco),2),
            `Mínimo` = round(min(Preco),2),
            `1º Quartil` = round(quantile(Preco, probs = .25),2),
            Mediana = round(quantile(Preco, probs = .5),2),
            `3º Quartil` = round(quantile(Preco, probs = .75),2),
            `Máximo` = round(max(Preco),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 
#GRAFICOS#

ggplot(vendas_prec_marc) +
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

vendas_masc_fem <- subset(vendas_masc_fem, !is.na(Categoria))
vendas_masc_fem <- subset(vendas_masc_fem, !is.na(Cor))


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
    x = freq, fill = Categoria, y = fct_reorder(Cor, freq, .desc = TRUE),
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = 0.45,
    size = 2.8,
    hjust = 0,
    angle = 0
  ) +
  xlim(0,80)+
  labs(x = "Frequência", y = "Cor") +
  theme_estat() 
ggsave("barras_categ_cor.pdf", width = 158, height = 110, units = "mm")

#TESTE DO QUI-QUADRADO#

tabela_contingencia <- table(vendas_masc_fem$Categoria, vendas_masc_fem$Cor)
resultado_qui_quadrado <- chisq.test(tabela_contingencia)
print(resultado_qui_quadrado)

##ANALISE 4##
#relação entre preço e avaliação#

vendas_prec_ava <- subset(vendas, !is.na(Preco))
vendas_prec_ava <- subset(vendas_prec_ava, !is.na(Avaliacao))

#GRAFICO#

ggplot(vendas_prec_ava) +
  aes(x = Preco, y = Avaliacao) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Preço em R$",
    y = "Avaliação"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")

#TABELA#

mean(vendas_prec_ava$Preco)
sd(vendas_prec_ava$Preco)
quantile(vendas_prec_ava$Preco)

mean(vendas_prec_ava$Avaliacao)
sd(vendas_prec_ava$Avaliacao)
quantile(vendas_prec_ava$Avaliacao)

#TESTE DE PEARSON#

cor.test(vendas_prec_ava$Preco, vendas_prec_ava$Avaliacao)

##ANALISE 5##
#frequencia de cada tipo de devolução por marca#

vendas_dev <- vendas %>% 
  subset(!is.na(Marca)) %>% 
  filter(Motivo.Devolucao != "Não Devolvido")

marca_dev <- vendas_dev %>%
  mutate(Marca = case_when(
    Marca %>% str_detect("Adidas") ~ "Adidas",
    Marca %>% str_detect("Gucci") ~ "Gucci",
    Marca %>% str_detect("H&M") ~ "H&M",
    Marca %>% str_detect("Nike") ~ "Nike",
    Marca %>% str_detect("Zara") ~ "Zara",
  )) %>%
  group_by(Marca, Motivo.Devolucao) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = scales::percent(freq / sum(freq))
  )

marca_dev <- marca_dev %>% 
  ungroup()

porcentagens2 <- str_c("(", str_replace_all(marca_dev$freq_relativa, "\\.", ","), ")")

legendas2 <- str_squish(str_c(marca_dev$freq, " ", porcentagens2))

ggplot(marca_dev) +
  aes(
    x = fct_reorder(Marca, freq, .desc = T), y = freq,
    fill = Motivo.Devolucao, label = str_c(freq, "\n", porcentagens2)
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.15, hjust = 0.5,
    size = 2.5
  ) +
  labs(x = "Marca", y = "Frequência", fill = "Motivo da devolução") +
  ylim(0,40) +
  theme_estat()
ggsave("marca_dev_freq.pdf", width = 158, height = 93, units = "mm")

#TESTE QUI-QUADRADO#

tabela_contingencia2 <- table(vendas_dev$Marca, vendas_dev$Motivo.Devolucao)
resultado_qui_quadrado2 <- chisq.test(tabela_contingencia2)
print(resultado_qui_quadrado2)

##ANALISE 6##

vendas_ava_marca <- subset(vendas, !is.na(Avaliacao))
vendas_ava_marca <- subset(vendas_ava_marca, !is.na(Marca))

media_ava <- vendas_ava_marca %>% 
  group_by(Marca) %>%
  summarize(Média = round(mean(Avaliacao),2))
