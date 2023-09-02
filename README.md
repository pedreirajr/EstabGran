<br>

**Autor:** Jorge Ubirajara Pedreira Junior  
**Data:** 20/07/2020

**1) Contexto**  
A estabilização granulométrica é o processo de mistura de dois ou mais
solos, visando obter um material com distribuição granulométrica
específica para compor uma camada de base ou sub-base de pavimentos.
Conforme preconiza o Manual de Pavimentação do Departamento Nacional de
Infraestrutura de Transportes (DNIT, 2006), a estabilização
granulométrica possibilita a obtenção de um material que pode ser
classificado de acordo com as curvas granulométricas A, B, C, D, E ou F,
cujos limites inferior e superior de material que passa em cada peneira
pode ser observado na tabela abaixo (DNIT, 2006):
<center>
![](TabelaDNIT.JPG)
</center>

De porte de materiais com características distintas, é possível escolher
a fração de cada material que deve compor a mistura final por meio
tentativa e erro ou abordagens algébricas e gráficas. Ao fim deste
processo, deve-se garantir que a mistura final atenda aos seguintes
requisitos geotécnicos (DNIT, 2006):

-   Limite de Liquidez do material que passa na peneira 40 ser menor do
    que 25;
-   Índice de Plasticidade do material que passa na peneira 40 ser
    inferior a 6;
-   Razão entre a quantidade de material que passa na peneira 200 e do
    material que passa na peneira 40 ser inferior a 2/3

Uma possível limitação dos métodos de tentativa, algébrico e gráfico
consiste no fato de não ser possível garantir, de antemão, que tais
propriedades geotécnicas sejam atendidas. Além disso, diferentes
materiais podem ter custos de obtenção distintos, implicando que certas
combinações, mesmo atingindo uma curva granulométrica de interesse e
atendendo às restrições geotécnicas impostas, sejam mais caras que
outras. É com base nestes dois fatores limitantes que o presente método
de otimização da mistura em estabilizações granulométricas é proposto.

**2) Método proposto**

O presente método consiste na resolução de um problema de otimização em
Programação Linear, com base no problema clássico da mistura (Hillier &
Lieberman, 2012). Em sua versão canônica, o problema da mistura envolve
uma função objetivo de minimização do custo final da mistura,
respeitando restrições técnicas relativas aos materiais envolvidos.  
Considerando as características apresentadas do problema da
estabilização granulométrica, em questão, o modelo de otimização com as
seguintes caracteríticas é proposto:

-   **Variáveis de decisão:**

*f*<sub>*i*</sub>: fração do material *i* na mistura

-   **Parâmetros:**

*c*<sub>*i*</sub>: custo unitário do material *i* (R$/kg).  
*p*<sub>*i*, *j*</sub>: percentual do material *i* que passa na peneira
*j*.  
*S*<sub>*j*</sub>: limite superior do percentual que passa na peneira
*j* para a faixa granulométrica desejada.  
*I*<sub>*j*</sub>: limite inferior do percentual que passa na peneira
*j* para a faixa granulométrica desejada.  
*L**L*<sub>*i*</sub>: limite de liquidez do material *i*.  
*I**P*<sub>*i*</sub>: índice de plasticidade do material *i*.  
*L**L*<sub>*m**a**x*</sub>: limite de liquidez máximo da mistura.  
*I**P*<sub>*m**a**x*</sub>: índice de plasticidade máximo da mistura.  
*R*<sub>\#200/\#40, *m**a**x*</sub>: razão máxima entre o percentual da
mistura que passa na peneira 200 e o percentual da mistura que passa na
peneira 40.

-   **Modelo:**

s.a.:

Reescrevendo as equações (4), (5) e (6) em um modelo linear tem-se:

**Referências**

\[1\] Departamento Nacional de Infraestrutura de Transportes (DNIT).
**Manual de Pavimentação**. Diretoria de Planejamento e Pesquisa.
Coordenação Geral de Estudos e Pesquisa. Instituto de Pesquisas
Rodoviárias. 2006.  
\[2\] Vargas, M. **Introdução à Mecânica dos Solos**. 1ª Ed. São Paulo:
MacGraw-Hill do Brasil. 1977.  
\[3\] Hillier, F. S.; Lieberman, G. J.**Introduction to Operations
Research**. McGraw-Hill, 2012.
