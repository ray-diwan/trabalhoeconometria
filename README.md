# Trabalho de Econometria I - FGV EESP - Raymond Diwan e Roni Mandelman
REPRODUÇÃO DO TRABALHO ENEM 2023

O pacote inclui o script em R, a base com os 1.045.050 participantes
elegíveis, as tabelas e os três gráficos do relatório.

1. Abra o script no RStudio. Defina a pasta extraída
   como diretório de trabalho (Session > Set Working Directory >
   To Source File Location).

2. Se necessário, instale os pacotes uma vez:
   install.packages(c("sandwich", "lmtest", "car"))

3. Rode o script inteiro com o botão Source ou pelo console:
   source("Analise_ENEM_2023.R", encoding = "UTF-8")

O script executa as etapas em ordem. Os objetos das regressões e das
tabelas ficam no Environment. Os CSVs e os gráficos gerados ficam em
minha_reproducao.

Para refazer também os filtros do trabalho, baixe os microdados do Inep,
extraia MICRODADOS_ENEM_2023.csv e indique o caminho na variável arquivo
no começo do script. Execute o script inteiro novamente. A Tabela 1
(seleção dos participantes) e o diagnóstico de comparecimento por renda
são gerados com o CSV original. As duas saídas já acompanham o pacote.

Também é possível indicar a entrada e a saída pelo terminal:
Rscript Analise_ENEM_2023.R "caminho/MICRODADOS_ENEM_2023.csv" minha_reproducao

Fonte dos microdados brutos
https://download.inep.gov.br/microdados/microdados_enem_2023.zip
