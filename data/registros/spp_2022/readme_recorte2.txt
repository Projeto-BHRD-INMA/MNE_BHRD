

Arquivos no link https://drive.google.com/drive/folders/1UPjw6GBB6_qI8sP13gt4wsLOlc1ZR0xc?usp=sharing

1_spp_recorte2 - lista de spp que ocorrem na bacia hidrográfica do rio Doce. 
Compilada por Talita. 234 espécies

2_Gbif_occ_recorte2 - registros brutos (raw) geográficos das spp da lista acima, 
retirados do GBIF, filtrado para apenas "preserved specimen".
Compilado por Danielle.
DOI: 10.15468/dl.34up3j
  Format: SIMPLE_CSV
  Download key: 0330953-210914110416597
  Created: 2022-06-01T18:23:23.114+00:00
  Modified: 2022-06-01T18:31:31.425+00:00
  Download link: https://api.gbif.org/v1/occurrence/download/request/0330953-210914110416597.zip
  Total records: 271060

3_Gbif_occ_recorte2_amesul -  registros de ocorrência (do arquivo 2_Gbif_occ_recorte2)
cortados para a América do Sul. Modificado por Danielle.
Total occurrences: 237643

4_gbif_recorte2_amesul_clean - tabela com os registros de ocorrência (do arquivo 3_Gbif_occ_recorte2_amesul)
cortados para a América do Sul, após a limpeza espacial com o uso do pacote CoordinateCleaner.
Realizado por Danielle. 229072 occurrences

5_recorte2_occ - registros de ocorrência (do arquivo 4_gbif_recorte2_amesul_clean), 
deixadas apenas as colunas de espécies, localidades e coordenadas.
Criado por Danielle.

6_recorte2_ModleR_out - registros geográficos (do arquivo 5_recorte2_occ), 
deixadas apenas as colunas de espécies e coordenadas e removidas as espécies com 30 ou menos registros de ocorrência.
Criado por Danielle. 229057 ocorrências
