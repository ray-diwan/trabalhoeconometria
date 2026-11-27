# 1. Leitura e seleção dos participantes

arquivo <- "resultados_enem/base_final.csv.gz"
pasta <- "minha_reproducao"

# Pelo terminal, também é possível indicar o arquivo e a pasta de saída.
if (sys.nframe() == 0L) {
  argumentos <- commandArgs(trailingOnly = TRUE)
  if (length(argumentos) >= 1L) arquivo <- argumentos[1L]
  if (length(argumentos) >= 2L) pasta <- argumentos[2L]
}

dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

salvar <- function(tabela, nome) {
  write.csv(tabela, file.path(pasta, paste0(nome, ".csv")), row.names = FALSE,
            fileEncoding = "UTF-8")
}

colunas <- c("TP_ST_CONCLUSAO", "IN_TREINEIRO", "TP_PRESENCA_MT", "TP_PRESENCA_LC",
             "NU_NOTA_MT", "NU_NOTA_LC", "TP_ESCOLA", "TP_FAIXA_ETARIA", "TP_SEXO",
             "TP_COR_RACA", "Q001", "Q002", "Q005", "Q006", "SG_UF_PROVA")

# A base filtrada acompanha o pacote. O CSV original usa ponto e vírgula.
if (endsWith(arquivo, ".gz")) {
  dados <- read.csv(arquivo, fileEncoding = "UTF-8")
} else {
  cabecalho <- names(read.csv(arquivo, sep = ";", nrows = 0, check.names = FALSE))
  classes <- ifelse(cabecalho %in% colunas, NA_character_, "NULL")
  dados <- read.csv(arquivo, sep = ";", colClasses = classes,
                    encoding = "latin1", check.names = FALSE)
}
stopifnot(is.data.frame(dados), all(colunas %in% names(dados)))
filtrada <- all(c("renda", "renda_detalhada", "privada") %in% names(dados))
bruto <- as.data.frame(dados)[, colunas]
rm(dados)
# Seleção da base: o modelo principal usa todos os elegíveis.
# 1. Concluintes do ensino médio, sem treineiros.
concluintes <- subset(bruto, TP_ST_CONCLUSAO == 2 & IN_TREINEIRO == 0)
# 2. Presença e nota registrada nas duas provas.
presentes <- subset(concluintes, TP_PRESENCA_MT == 1 & TP_PRESENCA_LC == 1 &
                      !is.na(NU_NOTA_MT) & !is.na(NU_NOTA_LC))
# 3. Manter a rede pública ou privada e os campos usados no modelo.
escolas <- subset(presentes, TP_ESCOLA %in% c(2, 3))
antes_zeros <- escolas[complete.cases(escolas) & escolas$Q006 %in% LETTERS[1:17], ]
# 4. Excluir nota zero em Matemática ou Linguagens.
dados <- subset(antes_zeros, NU_NOTA_MT > 0 & NU_NOTA_LC > 0)
n_elegiveis <- nrow(dados)
selecao <- data.frame(
  etapa = c("Registros carregados", "Concluintes sem treineiros", "Presentes com as duas notas",
            "Escola pública ou privada", "Campos válidos para a regressão",
            "Notas positivas nas duas", "Base usada no modelo"),
  n = c(nrow(bruto), nrow(concluintes), nrow(presentes), nrow(escolas),
        nrow(antes_zeros), n_elegiveis, nrow(dados))
)
if (!filtrada) {
  presenca <- with(concluintes, TP_PRESENCA_MT %in% 1 & TP_PRESENCA_LC %in% 1 &
                    !is.na(NU_NOTA_MT) & !is.na(NU_NOTA_LC))
  por_renda <- as.data.frame.matrix(table(concluintes$Q006, presenca))
  names(por_renda) <- c("sem_as_duas_notas", "com_as_duas_notas")
  salvar(data.frame(Q006 = rownames(por_renda), por_renda), "diagnostico_presenca_por_renda")
  salvar(selecao, "01_selecao")
}
rm(bruto, concluintes, presentes, escolas, antes_zeros)

# 2. Faixas de renda, controles e padronização das notas
faixas <- c("Até 1 SM", "Mais de 1 a 2 SM", "Mais de 2 a 4 SM",
            "Mais de 4 a 8 SM", "Acima de 8 SM")
grupos <- rep(faixas, c(2L, 2L, 3L, 4L, 6L))
dados$renda <- factor(grupos[match(dados$Q006, LETTERS[1:17])], levels = faixas)
dados$renda_detalhada <- factor(replace(as.character(dados$Q006), dados$Q006 %in% c("A", "B"), "AB"),
                                levels = c("AB", LETTERS[3:17]))
dados$privada <- as.integer(dados$TP_ESCOLA == 3L)
dados$mt <- as.numeric(scale(dados$NU_NOTA_MT))
dados$lc <- as.numeric(scale(dados$NU_NOTA_LC))
dados$dif <- dados$mt - dados$lc
controle <- c("TP_FAIXA_ETARIA", "TP_SEXO", "TP_COR_RACA", "Q001", "Q002", "SG_UF_PROVA")
dados[controle] <- lapply(dados[controle], factor)
if (!filtrada) {
  # 6. Guardar a base com os controles e a renda, sem repetir as notas padronizadas.
  con <- gzfile(file.path(pasta, "base_final.csv.gz"), "wt", encoding = "UTF-8")
  write.csv(dados[setdiff(names(dados), c("mt", "lc", "dif"))], con, row.names = FALSE)
  close(con)
}

# 3. Estatísticas descritivas

faixas <- levels(dados$renda)

limites <- data.frame(faixa = faixas, minimo = c(0, 1320.01, 2640.01, 5280.01, 10560.01),
                      maximo = c(1320, 2640, 5280, 10560, NA),
                      Q006 = c("A–B", "C–D", "E–G", "H–K", "L–Q"))
salvar(limites, "02_faixas")
variaveis <- dados[c("NU_NOTA_MT", "NU_NOTA_LC", "Q005", "privada")]
descritivas <- t(vapply(variaveis, function(x) {
  c(media = mean(x), dp = sd(x), variancia = var(x), minimo = min(x), maximo = max(x))
}, numeric(5)))
salvar(data.frame(variavel = rownames(descritivas), descritivas), "03_descritivas")
medias <- data.frame(
  faixa = faixas,
  n = as.integer(table(dados$renda)),
  mt = as.numeric(tapply(dados$NU_NOTA_MT, dados$renda, mean)),
  lc = as.numeric(tapply(dados$NU_NOTA_LC, dados$renda, mean)),
  privada = as.numeric(tapply(dados$privada, dados$renda, mean)),
  mt_z = as.numeric(tapply(dados$mt, dados$renda, mean)),
  lc_z = as.numeric(tapply(dados$lc, dados$renda, mean))
)
salvar(medias, "04_medias_por_renda")
correlacao <- cor(variaveis)
salvar(data.frame(variavel = rownames(correlacao), correlacao), "05_correlacao")

# Funções para resumir as regressões e calcular os intervalos

resultados_hc1 <- function(modelo, diagnostico = FALSE,
                           teste_conjunto = diagnostico,
                           vcov_hc1 = sandwich::vcovHC(modelo, type = "HC1"),
                           coeficientes = lmtest::coeftest(modelo, vcov. = vcov_hc1)) {

  resultado <- list(beta = coef(modelo), v = vcov_hc1,
                    coeficientes = coeficientes, n = nobs(modelo),
                    gl = df.residual(modelo), r2_aj = summary(modelo)$adj.r.squared)

  if (teste_conjunto) {
    # H0: todos os coeficientes das faixas de renda são iguais a zero.
    termos_renda <- grepl("^renda", names(coef(modelo)))
    restricoes <- diag(length(coef(modelo)))[termos_renda, , drop = FALSE]
    teste <- car::linearHypothesis(modelo, restricoes, vcov. = vcov_hc1, test = "F")
    resultado$teste_f <- data.frame(f = teste[2, "F"], gl1 = teste[2, "Df"],
                                    gl2 = teste[2, "Res.Df"], p = teste[2, "Pr(>F)"])
  }
  if (diagnostico) {
    bp <- lmtest::bptest(modelo, studentize = TRUE)
    resultado$bp <- data.frame(lm = unname(bp$statistic), gl = unname(bp$parameter),
                               p = bp$p.value)
  }
  resultado
}

resumo <- function(modelo, termos) {
  coefs <- modelo$coeficientes[termos, , drop = FALSE]
  b <- coefs[, 1]
  ep <- coefs[, 2]
  margem <- qt(.975, modelo$gl) * ep
  data.frame(termo = termos, beta = b, ep = ep, t = coefs[, 3], p = coefs[, 4],
             lo = b - margem, hi = b + margem, row.names = NULL)
}

# 4. Regressões principais e testes de hipótese

# lm estima o modelo. vcovHC calcula a matriz robusta. coeftest testa os coeficientes.
reg_mt <- lm(mt ~ renda + TP_FAIXA_ETARIA + TP_SEXO + TP_COR_RACA +
       Q001 + Q002 + SG_UF_PROVA + Q005 + privada, data = dados, singular.ok = FALSE)
vcov_mt <- sandwich::vcovHC(reg_mt, type = "HC1")
teste_mt <- lmtest::coeftest(reg_mt, vcov. = vcov_mt)
modelo_mt <- resultados_hc1(reg_mt, diagnostico = TRUE,
                             vcov_hc1 = vcov_mt, coeficientes = teste_mt)
rm(reg_mt)
reg_lc <- lm(lc ~ renda + TP_FAIXA_ETARIA + TP_SEXO + TP_COR_RACA +
       Q001 + Q002 + SG_UF_PROVA + Q005 + privada, data = dados, singular.ok = FALSE)
vcov_lc <- sandwich::vcovHC(reg_lc, type = "HC1")
teste_lc <- lmtest::coeftest(reg_lc, vcov. = vcov_lc)
modelo_lc <- resultados_hc1(reg_lc, diagnostico = TRUE,
                             vcov_hc1 = vcov_lc, coeficientes = teste_lc)
rm(reg_lc)
reg_dif <- lm(dif ~ renda + TP_FAIXA_ETARIA + TP_SEXO + TP_COR_RACA +
       Q001 + Q002 + SG_UF_PROVA + Q005 + privada, data = dados, singular.ok = FALSE)
vcov_dif <- sandwich::vcovHC(reg_dif, type = "HC1")
teste_dif <- lmtest::coeftest(reg_dif, vcov. = vcov_dif)
modelo_dif <- resultados_hc1(reg_dif, diagnostico = TRUE,
                             vcov_hc1 = vcov_dif, coeficientes = teste_dif)
rm(reg_dif)

stopifnot(isTRUE(all.equal(modelo_mt$beta - modelo_lc$beta, modelo_dif$beta,
                           tolerance = 1e-9)))
termos <- grep("^renda", names(modelo_dif$beta), value = TRUE)
fx <- c(termos, "privada")
# Juntar as linhas dos tres modelos na ordem em que aparecem no artigo.
principal <- rbind(
  data.frame(modelo = "mt", resumo(modelo_mt, fx)),
  data.frame(modelo = "lc", resumo(modelo_lc, fx)),
  data.frame(modelo = "dif", resumo(modelo_dif, fx))
)
ajuste <- data.frame(
  modelo = c("mt", "lc", "dif"),
  n = c(modelo_mt$n, modelo_lc$n, modelo_dif$n),
  parametros = c(length(modelo_mt$beta), length(modelo_lc$beta), length(modelo_dif$beta)),
  r2_aj = c(modelo_mt$r2_aj, modelo_lc$r2_aj, modelo_dif$r2_aj)
)
testes_f <- rbind(
  data.frame(modelo = "mt", modelo_mt$teste_f),
  data.frame(modelo = "lc", modelo_lc$teste_f),
  data.frame(modelo = "dif", modelo_dif$teste_f)
)
testes_dif <- resumo(modelo_dif, termos)
v_dif <- modelo_dif$v
superior <- termos[4L]
anterior <- termos[3L]
delta <- diff(modelo_dif$beta[c(anterior, superior)])
ep <- sqrt(sum(diag(v_dif)[c(anterior, superior)]) - 2 * v_dif[anterior, superior])
gl <- modelo_dif$gl
confronto <- data.frame(beta = unname(delta), ep = ep, p = 2 * pt(abs(delta / ep), gl, lower.tail = FALSE),
                        lo = unname(delta - qt(.975, gl) * ep), hi = unname(delta + qt(.975, gl) * ep))
salvar(principal, "06_modelo_completo")
salvar(ajuste, "06_ajuste_dos_modelos")
salvar(testes_dif, "07_testes_individuais")
salvar(testes_f, "07_testes_conjuntos")
salvar(confronto, "07_contraste_entre_as_faixas_superiores")

# 5. Comparação das especificações e diagnóstico de heterocedasticidade

modelo_so_renda <- resultados_hc1(lm(dif ~ renda, data = dados, singular.ok = FALSE))
formula_sem_rede <- dif ~ renda + TP_FAIXA_ETARIA + TP_SEXO + TP_COR_RACA +
  Q001 + Q002 + SG_UF_PROVA + Q005
modelo_sem_rede <- resultados_hc1(lm(formula_sem_rede, data = dados, singular.ok = FALSE))
especificacoes <- rbind(
  data.frame(modelo = "sem_controles", resumo(modelo_so_renda, termos),
             r2_aj = modelo_so_renda$r2_aj),
  data.frame(modelo = "sem_rede", resumo(modelo_sem_rede, termos),
             r2_aj = modelo_sem_rede$r2_aj),
  data.frame(modelo = "completo", resumo(modelo_dif, termos),
             r2_aj = modelo_dif$r2_aj)
)
hetero <- rbind(
  data.frame(modelo = "mt", modelo_mt$bp),
  data.frame(modelo = "lc", modelo_lc$bp),
  data.frame(modelo = "dif", modelo_dif$bp)
)
salvar(especificacoes, "08_especificacoes")
salvar(hetero, "09_koenker")

# 6. Análises de sensibilidade

modelo_detalhado <- resultados_hc1(
  lm(dif ~ renda_detalhada + TP_FAIXA_ETARIA + TP_SEXO + TP_COR_RACA +
       Q001 + Q002 + SG_UF_PROVA + Q005 + privada, data = dados, singular.ok = FALSE),
  teste_conjunto = TRUE
)
detalhados <- grep("^renda_detalhada", names(modelo_detalhado$beta), value = TRUE)
detalhe <- resumo(modelo_detalhado, detalhados)
detalhe$Q006 <- levels(dados$renda_detalhada)[-1L]
detalhe$n <- as.integer(table(dados$renda_detalhada)[-1L])
salvar(detalhe, "10_renda_detalhada")
salvar(modelo_detalhado$teste_f, "10_teste_conjunto_detalhado")

publico <- subset(dados, privada == 0L)
modelo_publico <- resultados_hc1(lm(formula_sem_rede, data = publico, singular.ok = FALSE))
tabela_publico <- resumo(modelo_publico, termos)
tabela_publico$n <- nrow(publico)
salvar(tabela_publico, "11_rede_publica")
perfil_coluna <- function(coluna, nome) {
  pct <- round(100 * prop.table(table(coluna)), 2)
  data.frame(variavel = nome, categoria = names(pct), pct = as.numeric(pct))
}
perfil <- rbind(
  perfil_coluna(dados$TP_SEXO, "TP_SEXO"),
  perfil_coluna(dados$TP_COR_RACA, "TP_COR_RACA"),
  perfil_coluna(dados$Q001, "Q001"),
  perfil_coluna(dados$Q002, "Q002")
)
salvar(perfil, "perfil")
pontos <- principal[principal$modelo %in% c("mt", "lc"), ]
dps <- c(mt = sd(dados$NU_NOTA_MT), lc = sd(dados$NU_NOTA_LC))
pontos$beta <- pontos$beta * dps[pontos$modelo]
pontos$ep <- pontos$ep * dps[pontos$modelo]
pontos$lo <- pontos$lo * dps[pontos$modelo]
pontos$hi <- pontos$hi * dps[pontos$modelo]
salvar(pontos, "magnitude_em_pontos")

# 7. Figuras do relatório
cores <- c(mt = "black", lc = "gray30")
figura <- function(nome, largura = 1500, altura = 750) {
  png(file.path(pasta, paste0(nome, ".png")), width = largura, height = altura,
      res = 160, bg = "white")
  par(mar = c(4.6, 4.5, 1.4, 1.1), las = 1, bty = "l")
}
legenda <- function(posicao, linhas = c(1, 2), pontos = c(NA, NA)) {
  legend(posicao, c("Matemática", "Linguagens"), col = cores, lty = linhas, pch = pontos,
         lwd = 2, bty = "n", cex = .93)
}
figura("Figura_1_distribuicoes")
densidade_mt <- density(dados$mt)
densidade_lc <- density(dados$lc)
plot(densidade_mt, xlim = range(dados[c("mt", "lc")]), ylim = c(0, max(densidade_mt$y, densidade_lc$y)),
     main = "", xlab = "Nota padronizada", ylab = "Densidade", col = cores["mt"], lwd = 2)
lines(densidade_lc, col = cores["lc"], lwd = 2, lty = 2)
legenda("topleft")
dev.off()

rotulos <- c("Até 1", ">1 a 2", ">2 a 4", ">4 a 8", ">8")
figura("Figura_2_medias_por_renda")
plot(1:5, medias$mt_z, type = "b", pch = 19, col = cores["mt"], lwd = 2, xaxt = "n",
     ylim = range(medias[c("mt_z", "lc_z")]), xlab = "Renda familiar em salários mínimos",
     ylab = "Nota média padronizada")
axis(1, at = 1:5, labels = rotulos)
abline(h = 0, col = "grey80", lty = 2)
lines(1:5, medias$lc_z, type = "b", pch = 17, col = cores["lc"], lwd = 2, lty = 2)
legenda("topleft", pontos = c(19, 17))
dev.off()

figura("Figura_3_coeficientes")
plot(NA, xlim = c(.65, 4.35), ylim = c(0, max(principal$hi[principal$modelo %in% c("mt", "lc")])),
     xaxt = "n", xlab = "Renda familiar em salários mínimos", ylab = "Coeficiente e IC de 95%")
axis(1, at = 1:4, labels = rotulos[-1])
# Deslocar um pouco os pontos para os intervalos das duas provas nao se sobreporem.
mt_grafico <- subset(principal, modelo == "mt" & termo %in% termos)
lc_grafico <- subset(principal, modelo == "lc" & termo %in% termos)
x_mt <- 1:4 - .07
x_lc <- 1:4 + .07
arrows(x_mt, mt_grafico$lo, x_mt, mt_grafico$hi, angle = 90,
       length = .04, code = 3, col = cores["mt"], lwd = 1.5)
points(x_mt, mt_grafico$beta, pch = 19, col = cores["mt"])
arrows(x_lc, lc_grafico$lo, x_lc, lc_grafico$hi, angle = 90,
       length = .04, code = 3, col = cores["lc"], lwd = 1.5)
points(x_lc, lc_grafico$beta, pch = 17, col = cores["lc"])
legenda("topleft", linhas = c(0, 0), pontos = c(19, 17))
dev.off()

print(selecao, row.names = FALSE)
print(testes_f, row.names = FALSE)
