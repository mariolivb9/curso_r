pdad_2015 <- read.csv2("~/curso_r/Base-de-Moradores-PDAD.csv", stringsAsFactors=FALSE)
Base_Moradores_PDAD_2015 <- read_delim("http://www.codeplan.df.gov.br/wp-content/uploads/2018/02/Base_Moradores_PDAD_2015.csv", 
                                       ";", escape_double = FALSE, trim_ws = TRUE)

##data table: mais rápido para carregar arquivos csv no R

library(data.table)
getwd()
pdad_moradores <- data.table::fread("/u01/u87252/Dados/Base-de-Moradores-PDAD.csv", 
                                    dec = ",",encoding = "Latin-1")
pdad_moradores <- data.table::fread("http://www.codeplan.df.gov.br/wp-content/uploads/2018/02/Base-de-Moradores-PDAD.csv",
                                    dec = ",",encoding = "Latin-1")

##Carga pelo banco de dados

# Carregar pacote
library(RODBC)

# Abrir conexão com o banco de dados
source("/u01/u87252/matricula.R")

db <- RODBC::odbcConnect("db_codeplan", uid=matricula, pwd=senha)

##removendo a senha
rm(matricula,senha)

# Verificar as tabelas disponíveis no banco de dados
tabelas <- RODBC::sqlTables(db)

#View(tabelas) # Visualizar o objeto tabelas
class(tabelas) # Verificar a classe do objeto
names(tabelas) # Verificar o nome das colunas carregadas
head(tabelas) # Verificar as primeiras linhas da tabela
tail(tabelas) # Verificar as últimas linhas da tabela
str(tabelas) # Verificar as classes das colunas
dplyr::glimpse(tabelas) # Outra opção para checar as classes
nrow(tabelas) # Consultar o número de linhas
ncol(tabelas) # Consultar o número de colunas

tabelas$TABLE_SCHEM # Consultar apenas a coluna "TABLE_SCHEM"
tabelas[,2] # Outra maneira de consultar a coluna "TABLE_SCHEM"
tabelas[,"TABLE_SCHEM"] # Mais uma maneira de consultar a coluna "TABLE_SCHEM"
tabelas[1:2,1:2] # Selecionar apenas as duas primeiras linhas e colunas
tabelas[1:2,]$TABLE_SCHEM # Selecionar apenas as duas primeiras linhas da coluna "TABLE_SCHEM"
tabelas[c(1,7,10:12),c(1,3)] # Selecionar linhar e colunas distintas

table(tabelas$TABLE_SCHEM) # Tabular a coluna
table(tabelas[,2]) # Tabular a coluna
table(tabelas[,"TABLE_SCHEM"]) # Tabular a coluna

tabelas[tabelas$TABLE_SCHEM=="pdad",3] ##filtro de tabelas que estão dentro da pdad, na coluna 3
tabelas[tabelas[,2]=='pdad',]$TABLE_NAME

# Carregar o pacote dplyr
library(dplyr)

# Consultar as tabelas disponíveis
RODBC::sqlTables(db) %>%
  # Filtrar apenas linhas do esquema PDAD
  dplyr::filter(TABLE_SCHEM=="pdad") %>%
  # Selecionar apenas a coluna com as tabelas
  dplyr::select(TABLE_NAME)

# Consultar as tabelas disponíveis OUTRO JEITO
  tabelas %>%
  # Filtrar apenas linhas do esquema PDAD
  filter(TABLE_SCHEM=="pdad") %>%
  # Selecionar apenas a coluna com as tabelas
  select(TABLE_NAME)
  
  # Alterar o nome da coluna "TABLE_SCHEM" para Esquema
  colnames(tabelas)[2] <- "Esquema"
  names(tabelas)[2] <- "Esquema"
  # Verificar o resultado
  names(tabelas)
  
  # Alterar o nome com o dplyr
  RODBC::sqlTables(db) %>%
    # Filtrar apenas linhas do esquema PDAD
    dplyr::filter(TABLE_SCHEM=="pdad") %>%
    # Alterar os nomes das colunas
    dplyr::rename(Nome=TABLE_NAME,
                  Banco=TABLE_CAT,
                  Esquema=TABLE_SCHEM,
                  Tipo=TABLE_TYPE,
                  Observações=REMARKS)

  # Consultar as tabelas disponíveis
  RODBC::sqlTables(db) %>%
    # Filtrar apenas linhas do esquema PDAD
    dplyr::filter(stringr::str_detect(TABLE_SCHEM, pattern =  "pdad")) %>%
    # Selecionar apenas a coluna com as tabelas
    dplyr::select(TABLE_NAME)
  
  # Consultar colunas
  colunas <- RODBC::sqlColumns(db,"pdad.pdad_2015_mor") 
  
  # Consultar as colunas da tabela pdad_2015_mor
  RODBC::sqlColumns(db,"pdad.pdad_2015_mor") %>%
    # Selecionar a coluna "COLUMN_NAME"
    dplyr::select(COLUMN_NAME)
  
  # Carregar a base de moradores da PDAD 2015
  pdad_moradores <- RODBC::sqlQuery(db,"select A01_DOM_RA,
                                    A05_DOM_MORADORES from 
                                    pdad.pdad_2015_mor")
  table(pdad_moradores$A01_DOM_RA)

  # Carregar a base completa de moradores
  pdad_moradores <- RODBC::sqlQuery(db,"select * from pdad.pdad_2015_mor",
                                    stringsAsFactors=F)  
  
  # Listar os arquivos do diretório dados
  list.files("/u01/u87252/Dados")
  # Carregar as informações do dicionário
  dic_moradores <- readxl::read_excel("/u01/u87252/Dados/Dicionário-Base-Moradores-PDAD-2.xlsx")
  
  # Carregar o pacote
  library(Hmisc)
  # Criar um objeto com os rótulos
  var.labels <- dic_moradores$`RÓTULO DA VARIÁVEL`
  # Nomear esses rótulos com o nome das variáveis do nosso banco de dados  
  names(var.labels) <- names(pdad_moradores)
  # Adicionar os rótulos ao nosso banco de dados
  pdad_moradores <- Hmisc::upData(pdad_moradores, labels = var.labels)
  # Verificar o resultado
  Hmisc::describe(pdad_moradores)
  
  # Construir uma variável em formato de fator
  RA <- pdad_moradores %>%
    dplyr::transmute(RA=factor(A01_DOM_RA,
                               levels=1:31,
                               labels=c('Brasília/Plano Piloto',      
                                        'Gama',
                                        'Taguatinga',
                                        'Brazlândia',
                                        'Sobradinho',
                                        'Planaltina',
                                        'Paranoá',
                                        'Núcleo Bandeirante',
                                        'Ceilândia',
                                        'Guará',
                                        'Cruzeiro',
                                        'Samambaia',
                                        'Santa Maria',
                                        'São Sebastião',
                                        'Recanto das Emas',
                                        'Lago Sul',
                                        'Riacho Fundo',
                                        'Lago Norte',
                                        'Candangolândia',
                                        'Águas Claras',
                                        'Riacho Fundo II',
                                        'Sudoeste/Octogonal',
                                        'Varjão',
                                        'Park Way',
                                        'SCIA-Estrutural',
                                        'Sobradinho II',
                                        'Jardim Botânico',
                                        'Itapoã',
                                        'SIA',
                                        'Vicente Pires',
                                        'Fercal')))
  # Tabular os resultados
  View(table(RA$RA))

  # Filtrar somente as informações da coluna RA
  ra <- dic_moradores[dic_moradores$`NOME DA VARIÁVEL`=="A01_DOM_RA",
                      ]$VALORES
  
  # Separar o vetor de string, baseado no argumento "\n"
  ra <- base::strsplit(ra, "\n")
  
  # Transformar a lista de caracteres em data.frame
  ra <- plyr::ldply(ra, data.frame, stringsAsFactors=F)
  
  # Renomear a coluna de RA
  colnames(ra) <- "RA"
  
  # Realizar os tratamentos finais
  ra <- ra %>%
    # Separar as colunas, baseado no espaço em branco ""
    tidyr::separate(col = RA, into = c("cod", "RA"), 
                    sep = " ",
                    extra="merge") %>%
    # Retirar o caracter "'" do campo RA e deixar somente a primeira letra maiúscula
    dplyr::mutate(RA= stringr::str_to_title(gsub("[']","",RA))) %>%
    # Criar uma coluna, unindo os valores com um sinal de "="
    tidyr::unite(col = "Arg",c("cod","RA"),sep="=",remove=F) %>%
    dplyr::mutate(RA=gsub("\r","",RA))
  
  
  # Recodificar os nomes
  ra_codificada <- pdad_moradores %>%
    dplyr::transmute(ra=factor(A01_DOM_RA,
                               levels = ra$cod,
                               labels = ra$RA)) %>%
    dplyr::select(ra)
  
  # Tabular os resultados
  table(ra_codificada$ra)
  
  RA %>%
    # Criar a área de plotagem, com o eixo X
    ggplot(aes(x=RA)) +
    # Inserir a geometrica do tipo "Barra", com a opção de contagem (gerada automaticamente no eixo y)
    geom_bar(stat = "count") +
    # Inverter os eixos
    coord_flip()

  # Contar quantas pessoas foram amostradas em cada RA
  RA %>%
    # Contar quantas observações temos em cada RA
    dplyr::count(RA) %>%
    # Plotar o gráfico, ajustando as categorias de acordo com o total amostrado
    ggplot(aes(x=forcats::fct_reorder(RA,n),y=n)) +
    # Desenhar a geometria de barras
    geom_bar(stat = "identity") +
    # Inverter os eixos
    coord_flip() +
    # Rotular os eixos
    labs(y="Amostra",
         x="Região Administrativa")  
  
  ggthemr::ggthemr()
  
  
  ##Juntando a base de moradores com a base de domicílios
  
  pdad_domicilios <- data.table::fread("/u01/u87252/Dados/Base-Domicílios-PDAD-2.csv", 
                                      dec = ",",encoding = "Latin-1")
  
  pdad_domicilios<-
    RODBC::sqlQuery(db, "select * from pdad.pdad_2015_dom")

  # Consultar o índice das colunas de mesmo nome entre as bases
  x <-
    which((names(pdad_domicilios) %in% names(pdad_moradores)))
  
  # Verificar quais são as colunas
  names(pdad_domicilios)[x]
  
  # Fazer o join das bases
  pdad <- pdad_moradores %>%
    # Entrar com a função para left join, n precisa colocar a chave pq ele já procura
    dplyr::left_join(
      # Informar a base que iremos unir, filtrando para colunas repetidas
      pdad_domicilios %>%
        dplyr::select(-c(A01_DOM_RA,A02_DOM_SETOR,A04_DOM_SETOR_CENSITARIO,
                         A05_DOM_MORADORES,NFATOR_RA,PESO_PRE,POPULACAO_AJUSTADA,
                         ESTRATO)),
      by=c("CD_SEQ_DOM"="CD_SEQ_DOM")) 
  
  # Carregar os pacotes necessários
  library(survey)
  library(srvyr)
  
  # Declarar o desenho incial
  sample.pdad <-  
    # Declarar a base a ser utilizada
    pdad %>%
    srvyr::as_survey_design(
      id = CD_SEQ_DOM, # Identificador único da unidade amostrada
      strata = ESTRATO, # Identificação do estrato
      weights = PESO_PRE, # Probabilidade da unidade ser sorteada
      nest=TRUE # Parâmetro de tratamento para dos IDs dos estratos
    )
  
  # Criar um objeto para pós estrato
  post.pop <- unique(subset(pdad,select=c(POPULACAO_AJUSTADA)))
  
  # Criar a variável de frequência (a mesma variável de pós-estrato, para funcionar como id e peso)
  post.pop$Freq <- post.pop$POPULACAO_AJUSTADA
  
  # Outra maneira de criar o mesmo objeto
  post.pop <- pdad %>%
    dplyr::select(POPULACAO_AJUSTADA) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Freq=POPULACAO_AJUSTADA)
  
  # Declarar o objeto de pós-estrato
  sample.pdad <- survey::postStratify(sample.pdad,~POPULACAO_AJUSTADA,post.pop)
  
  # Criar objeto para calcular os erros por bootstrap (Rao and Wu’s(n − 1) bootstrap)
  # J. N. K. Rao and C. F. J. Wu - Journal of the American Statistical Association
  # Vol. 83, No. 401 (Mar., 1988), pp. 231-241
  amostra <- survey::as.svrepdesign(sample.pdad, type = "subbootstrap")
  
  # Ajustar estratos com apenas uma UPA (adjust=centered)
  options( survey.lonely.psu = "adjust") #ajuste para uma única observação...perdi a explicação
  
  # Ajustar objeto de amostra, para uso com o pacote srvyr
  amostra <- srvyr::as_survey(amostra)
  
  # População DF com mais de 18 anos
  pop18 <- data.frame(
    amostra %>%
      # Filtrar somente a população com 18 anos ou mais de idade, retirando os códigos de não informação
      srvyr::filter(D06_MOR_IDADE>=18&D06_MOR_IDADE!=999) %>%
      # Criar uma variável auxiliar para contagem
      srvyr::mutate(count=1) %>%
      # Calcular o total da população, com seu intervalo de confiança
      srvyr::summarise(n=survey_total(count, vartype = "ci")))
  
  amostra %>%
    # Filtrar somente a população com 18 anos ou mais de idade, retirando os códigos de não informação
    srvyr::filter(D06_MOR_IDADE>=18&D06_MOR_IDADE!=999) %>%
    # Criar uma variável auxiliar para contagem
    # Informar o grupo que queremos a informação
    srvyr::group_by(D04_MOR_SEXO) %>%
    # Calcular o total e o Percentual da população, com seu intervalo de confiança
    srvyr::summarise(n=survey_total(vartype = "ci"),
                     # Calcular o percentual da população
                     pct=survey_mean(vartype = "ci"))
  
  
  #######5.2
  
  # Utilizar a base de domicílios
  pdad_domicilios %>%
    # Selecionar apenas a variável de data
    dplyr::select(DT_DATA_PESQUISA) %>%
    # Contar o número de caracteres do campo data
    dplyr::mutate(n_char=stringr::str_length(DT_DATA_PESQUISA)) %>%
    # Contar a quantidade de caracteres
    dplyr::count(n_char)
  
  glimpse(pdad_domicilios)
  
  # Utilizar a base de domicílios
  pdad_domicilios %>%
    # Selecionar apenas a variável de data
    dplyr::select(A01_DOM_RA,DT_DATA_PESQUISA) %>%
    # Contar o número de caracteres do campo data
    dplyr::mutate(ano_pesquisa=stringr::str_sub(DT_DATA_PESQUISA,7,10)) %>%
    # Contar a quantidade de caracteres
    dplyr::count(A01_DOM_RA,ano_pesquisa) 
  
  # Utilizar a base de domicílios
  pdad_domicilios %>%
    # Selecionar apenas a variável de data
    dplyr::select(DT_DATA_PESQUISA) %>%
    # Contar o número de caracteres do campo data
    dplyr::mutate(n_char=stringr::str_length(stringr::str_trim(DT_DATA_PESQUISA))) %>%
    # Contar a quantidade de caracteres
    dplyr::count(n_char)   
  
  
  # Amrzenar resultado no objeto x
  x<-
    # Utilizar a base de domicílios
    pdad_domicilios %>%
    # Selecionar a data da pesquisa
    dplyr::select(DT_DATA_PESQUISA) %>%
    # Retirar os espaços em branco
    dplyr::mutate(DT_DATA_PESQUISA=stringr::str_trim(DT_DATA_PESQUISA)) %>%
    # Separar a data, em MM, DD, AAAA, utilizando como separador "/" e mantendo a variável original
    tidyr::separate(DT_DATA_PESQUISA,
                    into = c("MM","DD","AAAA"),sep="/",
                    remove=F) %>%
    # Ajustar o mês e o ano, adicionando um zero à esquerda quando houver somente 1 caracter, criado a variável de data no formato DD/MM/AAAA
    #paste0: não coloca um separador entre os strings
    #paste: coloca o separador
    dplyr::mutate(MM=case_when(stringr::str_length(MM)==1~paste0("0",MM),
                               TRUE~MM),
                  DD=case_when(stringr::str_length(DD)==1~paste0("0",DD),
                               TRUE~DD),
                  DATA_AJUSTADA=paste(DD,MM,AAAA,sep = "/"),
                  ANO=stringr::str_sub(DATA_AJUSTADA,7,10))
  
  View(table(x$ANO))

  ##programação mais simples 
  
  # Utilizar a base de domicílios
  pdad_domicilios %>%
    # Selecionar a data da pesquisa
    dplyr::select(DT_DATA_PESQUISA) %>%
    # Transformar o campo de data (em caracter) em data (formato data)
    #mdy: mês, dia, ano (é possível trocar a ordem se necessário)
    dplyr::mutate(DATA_CORRIGIDA=lubridate::mdy(DT_DATA_PESQUISA),
                  # Extrair o valor do ano
                  ANO=lubridate::year(DATA_CORRIGIDA))  

  # Utilizar a base de domicílios
  pdad_domicilios %>%
    # Selecionar a data da pesquisa
    dplyr::select(CD_SEQ_DOM,A01_DOM_RA,DT_DATA_PESQUISA) %>%
    # Transformar o campo de data (em caracter) em data (formato data)
    dplyr::mutate(DATA_CORRIGIDA=lubridate::mdy(DT_DATA_PESQUISA),
                  # Extrair o valor do ano
                  ANO=lubridate::year(DATA_CORRIGIDA)) %>%
    # Verificar o total de obsevações por ano
    dplyr::count(A01_DOM_RA,ANO)  

  # Construir uma variável em formato de fator
  pdad_domicilios <- pdad_domicilios %>%
    dplyr::mutate(RA=factor(A01_DOM_RA,
                               levels=1:31,
                               labels=c('Brasília/Plano Piloto',      
                                        'Gama',
                                        'Taguatinga',
                                        'Brazlândia',
                                        'Sobradinho',
                                        'Planaltina',
                                        'Paranoá',
                                        'Núcleo Bandeirante',
                                        'Ceilândia',
                                        'Guará',
                                        'Cruzeiro',
                                        'Samambaia',
                                        'Santa Maria',
                                        'São Sebastião',
                                        'Recanto das Emas',
                                        'Lago Sul',
                                        'Riacho Fundo',
                                        'Lago Norte',
                                        'Candangolândia',
                                        'Águas Claras',
                                        'Riacho Fundo II',
                                        'Sudoeste/Octogonal',
                                        'Varjão',
                                        'Park Way',
                                        'SCIA-Estrutural',
                                        'Sobradinho II',
                                        'Jardim Botânico',
                                        'Itapoã',
                                        'SIA',
                                        'Vicente Pires',
                                        'Fercal')))
  
  inconsistencia <-
    # Utilizar a base de domicílios
    pdad_domicilios %>%
    # Selecionar a data da pesquisa
    dplyr::select(CD_SEQ_DOM,RA,DT_DATA_PESQUISA) %>%
    # Transformar o campo de data (em caracter) em data (formato data)
    dplyr::mutate(DATA_CORRIGIDA=lubridate::mdy(DT_DATA_PESQUISA),
                  # Extrair o valor do ano
                  ANO=lubridate::year(DATA_CORRIGIDA)) %>%
    # Verificar o total de obsevações por ano
    dplyr::filter(!ANO %in% c(2015,2016)) %>%
    # Organizar os resultados por ordem crescente de RA e Data
    dplyr::arrange(RA,CD_SEQ_DOM)  
  
  write.table(inconsistencia,"inconsistencia.csv",
              row.names = F, sep = ";")
  
  write.table(inconsistencia,"inconsistencia.csv",
              row.names = F, sep = ";",
              fileEncoding = "Latin1",
              na = "")

  library(lubridate)
  # Criar um objeto com a data de referência da pesquisa
  ref <- dmy("15-07-2015")
  
  # Armazenar o resultado em um objeto
  referencias <- 
    # Utilizar a base de domicílios
    pdad_domicilios %>%
    # Selecionar a data da pesquisa
    dplyr::select(DT_DATA_PESQUISA) %>%
    # Transformar o campo de data (em caracter) em data (formato data)
    dplyr::mutate(DATA_CORRIGIDA=lubridate::mdy(DT_DATA_PESQUISA),
                  # Calcular a diferença de datas em meses
                  dif_data_mes=interval(ref,DATA_CORRIGIDA) %/% months(1),
                  # Calcular a diferença de datas em dias
                  dif_data_dia=interval(ref,DATA_CORRIGIDA) %/% days(1)) %>%
    # Filtrar as datas problemáticas
    dplyr::filter(year(DATA_CORRIGIDA) %in% c(2015,2016)) 
  
  # Utilizar a base de referências
  referencias %>%
    # Criar um plot com a diferença de dias no eixo x
    ggplot(aes(x=dif_data_dia)) +
    # Fazer o gráfico de densidade
    geom_density() +
    # Adicionar uma linha vermelha vertical no ponto zero
    geom_vline(aes(xintercept= 0), color="red")+
    # Nomear os eixos
    labs(y="Densidade",x="Diferença de dias da pesquisa")

  
  # Criar um objeto com o salário mínimo
  sm <- 788
  
  # Criar um objeto com as variáveis de interesse
  x <- amostra %>%
    # Criar variável de sexo
    srvyr::mutate(sexo=case_when(D04_MOR_SEXO==1~"Masculino",
                                 D04_MOR_SEXO==2~"Feminino"),
                  # Criar variável de esgotamento sanitário
                  esgotamento=case_when(B07_DOM_ESGOTO_SANITARIO==1~"Rede Geral (Caesb)",
                                        B07_DOM_ESGOTO_SANITARIO==2~"Fossa séptica",
                                        B07_DOM_ESGOTO_SANITARIO==3~"Fossa rudimentar",
                                        B07_DOM_ESGOTO_SANITARIO==4~"Esgoto a céu aberto",
                                        TRUE~"Outros"),
                  # Criar variável de faixas de idade
                  idade_faixas=cut(D06_MOR_IDADE,
                                   #-Inf: menor valor
                                   breaks = c(-Inf,seq(4,79,by=5),Inf), 
                                   labels = c("0 a 4 anos","5 a 9 anos","10 a 14 anos",
                                              "15 a 19 anos","20 a 24 anos",
                                              "25 a 29 anos","30 a 34 anos",
                                              "35 a 39 anos","40 a 44 anos",
                                              "45 a 49 anos","50 a 54 anos",
                                              "55 a 59 anos","60 a 64 anos",
                                              "65 a 69 anos","70 a 74 anos",
                                              "75 a 79 anos","Mais de 80 anos"),
                                   ordered_result = T),
                  # Criar variável de faixas de salário
                  #NA_real_: Missing numérico
                  faixas_salario=cut(case_when(E14_MOR_PRINC_REND_BRUTO %in% c(77777,88888,99999)~NA_real_,
                                               
                                               TRUE~as.numeric(E14_MOR_PRINC_REND_BRUTO)),
                                     breaks = c(-Inf,sm,2*sm,4*sm,10*sm,Inf),
                                     labels = c("Até 1 salário","Mais de 1 até 2 salários",
                                                "Mais de 2 até 4 salários",
                                                "Mais de 4 até 10 salários",
                                                "Mais de 10 salários")),
                  # Criar variável para as RAs
                  RA=factor(A01_DOM_RA,
                            levels=1:31,
                            labels=c('Brasília/Plano Piloto',      
                                     'Gama',
                                     'Taguatinga',
                                     'Brazlândia',
                                     'Sobradinho',
                                     'Planaltina',
                                     'Paranoá',
                                     'Núcleo Bandeirante',
                                     'Ceilândia',
                                     'Guará',
                                     'Cruzeiro',
                                     'Samambaia',
                                     'Santa Maria',
                                     'São Sebastião',
                                     'Recanto das Emas',
                                     'Lago Sul',
                                     'Riacho Fundo',
                                     'Lago Norte',
                                     'Candangolândia',
                                     'Águas Claras',
                                     'Riacho Fundo II',
                                     'Sudoeste/Octogonal',
                                     'Varjão',
                                     'Park Way',
                                     'SCIA-Estrutural',
                                     'Sobradinho II',
                                     'Jardim Botânico',
                                     'Itapoã',
                                     'SIA',
                                     'Vicente Pires',
                                     'Fercal'))) %>%
    # Transformar em fator variáveis do tipo character
    srvyr::mutate_if(is.character,funs(factor(.))) %>%
    # Selecionar as variáveis criadas e algumas variáveis auxiliares
    srvyr::select(RA,D03_MOR_CONDICAO_UNID,D06_MOR_IDADE,E08_MOR_ATIVIDADE,sexo,esgotamento,idade_faixas,faixas_salario)  
  
  
  # Construir um objeto com as idades calculadas, por faixas de idade e sexo
  piramide <-
    x %>%
    # Retirar as idades indeterminadas
    srvyr::filter(D06_MOR_IDADE!=999) %>%
    # Agrupar por faixas de idade e sexo
    srvyr::group_by(idade_faixas,sexo) %>%
    # Calcular os totais
    srvyr::summarise(n=survey_total(na.rm = T, vartype = "ci")) #ci: intervalo de confiança
  
  # Fazer o gráfico com a pirâmide
  piramide_grafico <-
    piramide %>%
    # Construir um plot com as idades no eixo x, as quantidades no eixo y,
    #  preenchimento com a variável sexo, e os intervalos de confiança
    # inferiores e superiores
    ggplot(aes(x=idade_faixas,y=n, fill=sexo, ymin=n_low,ymax=n_upp))+
    # Fazer o gráfico de barras para o sexo Feminino
    geom_bar(data = dplyr::filter(piramide, sexo == "Feminino"),
             stat = "identity") +
    # Fazer o gráfico de barras para o sexo Masculino
    geom_bar(data = dplyr::filter(piramide, sexo == "Masculino"),
             stat = "identity",
             position = "identity",
             # Negativar os valores para espelhar no eixo
             mapping = aes(y = -n))+
    # Plotar os erros para o sexo Masculino, negativando os valores para espelhar o eixo
    geom_errorbar(data = dplyr::filter(piramide, sexo == "Masculino"),
                  mapping = aes(ymin = -n_low,ymax=-n_upp),
                  width=0,
                  color="black")+
    # Plotar os erros para o sexo Feminino
    geom_errorbar(data = dplyr::filter(piramide, sexo == "Feminino"),
                  width=0,
                  color="black")+
    # Inverter os eixos, fazendo com que o gráfico de colunas verticais fique
    # horizontal
    coord_flip() + 
    # Ajustar as configurações de escala (tava em notação científica)
    scale_y_continuous(labels = function(x) format(abs(x), 
                                                   big.mark = ".", #separador de milhar
                                                   scientific = FALSE, #sem notação científica
                                                   decimal.mark=",")) +
    # Suprimir os nomes dos eixos
    labs(x="",y="") +
    # Suprimir o nome da legenda
    scale_fill_discrete(name = "")
  
  # Plotar gráfico
  piramide_grafico
  
  # Construir um objeto com as informações de salario
  salario <- x %>%
    # Filtar pessoas sem atividade
    srvyr::filter(E08_MOR_ATIVIDADE!=0) %>%
    # Agrupar por faixas de salário
    srvyr::group_by(faixas_salario) %>%
    # Calcular os totais para cada grupo de salário
    srvyr::summarise(n=survey_total(na.rm=T,vartype = "ci"))
  
  # Construir um objeto com o gráfico
  salario_grafico <-
    salario %>%
    # Plotar os eixos x e y
    ggplot(aes(x=faixas_salario, y=n))+
    # Construir o gráfico de barras
    geom_bar(stat = "identity") +
    # Construir as barras de erro
    geom_errorbar(aes(ymin=n_low,ymax=n_upp,size=4, width=0), color="darkred")+
    # Inverter os eixos
    coord_flip()+
    # Suprimir o nome dos eixos
    labs(x="",y="")+
    # Retirar o título da legenda
    theme(legend.position="none")+
    # Ajustar as formatações de escala
    scale_y_continuous(labels = function(x) format(abs(x), 
                                                   big.mark = ".",
                                                   scientific = FALSE,
                                                   decimal.mark=","))
  
  # Plotar gráfico
  salario_grafico
  
  # Carregar o pacote Scales
  library(scales)
  
  # Construir o objeto com os valores
  salario2 <- x %>%
    # Retirar pessoas sem atividade
    srvyr::filter(E08_MOR_ATIVIDADE!=0) %>%
    # Agrupar por RA e faixas de salário
    srvyr::group_by(RA,faixas_salario) %>%
    # Calcular as proporções por faixa de salário
    srvyr::summarise(n=survey_mean(na.rm=T,vartype = "ci"))
  
  # Construir o gráfico
  salario2 %>%
    # Plotar os eixos x e y
    ggplot(aes(x=faixas_salario, y=n))+
    # Construir o gráfico de barras
    geom_bar(stat = "identity") +
    # Construir o gráfico com os erros
    geom_errorbar(aes(ymin=n_low,ymax=n_upp,size=4, width=0,group=RA), color="darkred")+
    # Inverter os eixos
    coord_flip()+
    # Suprimir o nome dos eixos
    labs(x="",y="")+
    # Suprimir o nome da legenda
    theme(legend.position="none")+
    # Ajustar as formatações de escala
    scale_y_continuous(labels = scales::percent)+
    # Plotar o gráfico para cada uma das RAs, divididas em 4 colunas
    facet_wrap(.~RA, ncol=4)

  library(stringr)
  # Consultar as tabelas do esquema inflação
  RODBC::sqlTables(db) %>%
    dplyr::filter(str_detect(TABLE_SCHEM,"inflacao"))
  
  # Consular as colunas da tabela do IPCA mensal
  RODBC::sqlColumns(db,"inflacao.ipca_mensal")$COLUMN_NAME
  
  # Puxar as colunas desejadas
  #codigo igual a zero é o índice geral da inflação
  inflacao_df <- RODBC::sqlQuery(db,"select referencia,codigo,descricao,distritofederal from inflacao.ipca_mensal where codigo=0",stringsAsFactors=F) %>%
    # Ajustar a variável de data
    dplyr::mutate(referencia=lubridate::ymd_hms(referencia)) %>%
    # Filtrar datas superiores ao período de referência da PDAD 2015
    dplyr::filter(referencia>"2015-07-01")
  View(inflacao_df)
  
  # Calcular a inflação acumulada
  inflacao_acum <- prod(inflacao_df$distritofederal/100+1)
  
  # Calcular o salário médio nominal e ajustado da PDAD 2015
  amostra %>%
    # Retirar pessoas sem atividade
    srvyr::filter(E08_MOR_ATIVIDADE!=0) %>%
    # Criar uma variável com o salário nominal e o salário real
    srvyr::mutate(salario_nominal=case_when(E14_MOR_PRINC_REND_BRUTO %in%
                                              c(77777,88888,99999)~NA_real_,
                                            TRUE~as.numeric(E14_MOR_PRINC_REND_BRUTO)),
                  salario_real=salario_nominal*inflacao_acum) %>%
    # Calcular os valores médios
    srvyr::summarise(salario_nominal=survey_mean(salario_nominal, na.rm=T,vartype = "ci"),
                     salario_real=survey_mean(salario_real, na.rm=T,vartype = "ci"))
  
  inflacao <- RODBC::sqlQuery(db,"select referencia,descricao,distritofederal,riodejaneiro,saopaulo,brasil from inflacao.ipca_acum12m where codigo=0",stringsAsFactors=F) %>%
    # Ajustar a variável de data
    dplyr::mutate(referencia=lubridate::ymd_hms(referencia)) %>%
    # Filtrar datas superiores ao período de referência da PDAD 2015
    dplyr::filter(referencia>"2017-06-01")
  
  inflacao_long <-
    inflacao %>%
    # Passar as colunas de cada localidade para o formato long, sendo
    # a localidade armazenada na variável "Local" e os valores na variável
    # "Inflação"
    tidyr::gather("Local","Inflação",c(3:6))

  inflacao<-
    # Retornar do formado long para o formato wide
    inflacao_long %>%
    tidyr::spread(Local,`Inflação`)

  inflacao %>%
    # Passar as colunas de cada localidade para o formato long, sendo
    # a localidade armazenada na variável "Local" e os valores na variável
    # "Inflação"
    tidyr::gather("Local","Inflação",c(3:6)) %>%
    # Construir o gráfico, com a referencia no eixo x (mudando para o formato data),
    # a inflação no eixo y e o Local na cor
    ggplot(aes(x=ymd(referencia),y=`Inflação`/100,colour=Local))+
    # Construir as linhas, variando o tipo de linha conforme o local
    geom_line(aes(linetype = Local))+
    # Ajustar os rótudos dos meses do eixo x
    scale_x_date(date_breaks = "2 month")+
    # Ajustar o rótulo do eixo y
    scale_y_continuous(labels = scales::percent)+
    # Ajustar a legenda das cores, atribuindo cores específicas para as linhas
    scale_colour_manual(labels=c("Brasil","Distrito Federal",
                                 "Rio de Janeiro","São Paulo"),
                        values=c("cadetblue4","coral4",
                                 "darkgoldenrod","chartreuse4"))+
    # Ajustar a legenda das linhas, combinando com a legenda anterior
    scale_linetype_manual(labels=c("Brasil","Distrito Federal",
                                   "Rio de Janeiro","São Paulo"), 
                          values=c(1:4))+
    # Ajustar o rótulo dos eixos
    labs(y="Inflação acumulada (12 meses)",
         x="Período")+
    # Alterar a posição da legenda
    theme(legend.position = "bottom")  
  
  # Construir o objeto com o esgotamento sanitário
  esgotamento <- x %>%
    # Filtrar para as informações somente do responsável (1 obs. por domicílio)
    srvyr::filter(D03_MOR_CONDICAO_UNID==1) %>%
    # Agrupar por situação de esgotamento sanitário
    srvyr::group_by(esgotamento) %>%
    # Calcular a proporção de cada grupo
    srvyr::summarise(n=survey_mean(na.rm = T,vartype = "ci"))
  
  library(forcats)
  
  # Construir o objeto com o gráfico
  esgotamento_grafico <-
    esgotamento %>%
    # Plotar os eixos x e y, reordenando os fatores, do maior para o menor resultado
    #-n: do maior pro menor
    ggplot(aes(x=fct_reorder(esgotamento,-n),y=n,ymin=n_low,ymax=n_upp))+
    # Construir o gráfico de barras
    geom_bar(stat = "identity")+
    # Construir os erros
    geom_errorbar(size=4, width=0,
                  color="black")+
    # Ajustar os nomes dos eixos
    labs(x="",y="Nº Domicílios")+
    # Retirar o nome da legenda
    theme(legend.position="none")+
    # Ajustar a formatação dos rótulos
    scale_y_continuous(labels = scales::percent)+
    # Inserir informações dos resultados no gráfico
    geom_text(aes(label = paste0(round(100*n,0),"%")),
              size=3, fontface = "bold", 
              vjust = -0.25,hjust=1.25)
  
  # Plotar grafico
  esgotamento_grafico
  
  
  # Carregar o pacote ggrepel
  library(ggrepel)
  
  # Construir o objeto com as informações de esgotamento sanitário
  esgotamento2 <- x %>%
    # Filtar para o responsável
    srvyr::filter(D03_MOR_CONDICAO_UNID==1) %>%
    # Agrupar por tipo de esgotamento
    srvyr::group_by(esgotamento) %>%
    # Calcular as proporções
    srvyr::summarise(n=survey_mean(na.rm = T,vartype = "ci")) %>%
    # Deixar as informações em ordem decrescente
    dplyr::arrange(-n) %>%
    # Construir uma variável auxiliar, com a posição do label
    dplyr::mutate(pos=cumsum(n)-n/8)
  
  # Criar o tema branco, eliminando todos os elementos gráficos padrões
  tema_branco <- theme_minimal()+
    theme(
      # Retirar título do eixo x
      axis.title.x = element_blank(),
      # Retirar título do eixo y
      axis.title.y = element_blank(),
      # Retirar as bordas no painel
      panel.border = element_blank(),
      # Retirar elementos textuais do eixo y
      axis.text.y = element_blank(),
      # Retirar demais elementos textuais dos eixos
      axis.text = element_blank(),
      # Retirar as linhas de grade
      panel.grid=element_blank(),
      # Retirar os ticks
      axis.ticks = element_blank())
  
  # Construir o gráfico de pizza
  esgotamento2 %>%
    # Plotar o gráfico, com as quantidades no eixo y, o preenchimento com as categorias,
    # reordenando as quabtudades, e o valor 1 para travar o eixo x
    ggplot(aes(x=1,y=n,fill=fct_reorder(esgotamento,n)))+
    # Construir as "barras"
    geom_bar(stat="identity")+
    # Transformar em coordenada polar o eixo y, com início em 0
    coord_polar("y", start=0)+
    # Retirar os nomes dos eixos
    labs(x="",y="") +
    # Adicionar o tema branco
    tema_branco+
    # Retirar o nome da legenda
    scale_fill_discrete(name="")+
    # Adicionar o label com os valores, usando a função repel para evitar
    # sobreposições
    geom_text_repel(aes(label = percent(n), y=pos), size=5, color="white",
                    fontface="bold")
  