# Objetivos
   - testar algoritmos ao vivo
   - forma fácil de trocar os protocolos
   - forma de os desenvolver



# Testar algoritmos ao vivo

- era engraçado ter uma interface para
    - começar N nodes
    - escolher o protocolo
    - na interface também deve aparecer os parametros iniciais do protocolo

ou seja, fazer um programa para que seja fácil começar vários nodes (localmente ou arranjar forma de os começar em remoto), escolher o protocolo, costumizar o protocolo e "simular" transações entre eles
para tal, acho que é preciso que as transações (monetárias) tenham de ser iguais entre todos os protocolos, no entanto, eles podem ter outras transações, mas têm de ter umas que servem só para testar
ou seja, as transações têm de implementar uma interface em específico para que a nossa plataforma consiga os usar

mesmo que os Nodes comuniquem outro tipo de mensagens (por exemplo, Delegation, ou votações, etc), têm de haver uma em específico que serve para contabilizar

também há outros fatores para comparar e contabilizar (talvez, isto também deve ser implementado como uma interface pelo próprio Protocolo), como se fossem estatísticas

fatores a contabilizar/fazer estatísticas:
- consensus complexity (o número de mensagens necessárias para chegar a um consenso) por node
- finalization time
- TPS/Throughput
- Scalability
- Nodes to fail
- O protocolo deve ter outras estatísticas que são independentes da plataforma (mas demonstradas na mesma)

## Fazer prático
- Fazer um servidor web que sirva isto tudo
- Fazer páginas para estatísitcas
- Fazer página para escolha de protocolos
- Fazer página para escolher os parâmetros do protocolo
- Alguma cena para guardar mapear estas combinações às estatísticas
- Pensar como é que as estatísticas são calculadas, se vemos só o que está num nó ou "na rede"


# Forma fácil de trocar os protocolos
Isto deve estar dentro da plataforma, mas mais trabalho terá que ser feito para que eles sejam integraldos
A pasta `proto_custom_demo` já está incluída da pipeline de compilação da Tezos, logo, "mudar" o protocolo seria apenas um caso de trocar os ficheiros que lá estão


# Forma de os desenvolver
Há um número de bibliotecas/libs/modules que são iguais a todos os protocolos
   - path_encoding.ml
   - storage_costs.ml
   - storage_description.ml
   - storage_functors.ml
   - storage_functors.mli
   - storage_sigs.ml

Há um número de .... que estão incluídos em vários protocolos mas que diferem entre si
Ou seja, nestes está definida a lógica do consenso

- alpha_context.ml - Pode estar incluído ou não - Representa a visão do contexto atual por parte do consenso
- apply.ml - Pode estar incluído ou não (pode estar representado na main) - aplica o bloco ao contexto atual
- block_header_repr.ml - tem de estar incluído (mesmo que só no main) - representação do header por parte do protocolo
- constants_repr.ml - tem de estar incluído (e até pode ser generalizado) -  representação das constantes paramétricas/normais
- init_storage.ml - tem de estar incluído (mesmo que só no main) - representação da lógica do começo do protocolo/preparação do protocolo
- level_repr.ml - representação do nível
- main.ml - depende do environemnt - único ficheiro requirido pelo protocolo
- operation_repr.ml - tem de estar incluído, porque todos os protocolos têm operações
- parameters_repr.ml - tem de estar incluído, mas tenho de arranjar uma forma de a plataforma perceber isto
- raw_context.ml - Pode estar incluído ou não - Representa a visão do contexto atual por parte da lógica de armazenamento/protocolo como programa
- services.ml - Tem de estar incluído, mas tenho de arranjar uma forma de a plataforma perceber isto
- services_registration.ml - igual a services.ml
- storage.ml - tem de estar incluído, mas depende de cada protocolo - básicamente faz uso dos outros ficheiros "storage", mas é fácil de perceber. Não passa de representar como é que cada valor vai ser armazenado na storage, como, por exemplo, para obtermos o saldo de uma conta nós temos de passar a chave dessa conta

Os TEMPLATES também são iguais a todos os protocolos
(também temos de ver uma forma de generalizar os environments)
