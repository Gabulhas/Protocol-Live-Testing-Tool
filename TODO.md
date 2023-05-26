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

   - account_repr.ml
   - manager_repr.ml
   - tez_repr.ml

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



It seems like you have done quite a lot of work already. The "protocol_helper" library and the functor that returns the block header module sound like good steps towards your goal of facilitating the development, testing, and comparison of consensus protocols.

Here are some more ideas that might help you:

1. **Testing Framework:** It might be beneficial to create a standard testing framework that can be used to evaluate the performance of different consensus protocols. This could include stress tests, unit tests, and integration tests. For example, stress tests could simulate high transaction volume, network latency, and other extreme conditions to see how the protocol performs.

2. **Benchmarking Suite:** Alongside a testing framework, a benchmarking suite could help to measure and compare the performance of different consensus protocols. This could include metrics like transaction throughput, latency, and resource usage (CPU, memory, network bandwidth).

3. **Standardized Interface:** By defining a standardized interface for consensus protocols, you could make it easier to switch between different protocols. This interface could define methods for common operations like validating a block, creating a new block, and selecting the next leader.

4. **Protocol Templates:** You could create templates for common types of consensus protocols (like Proof of Work, Proof of Stake, Byzantine Fault Tolerance). This could save developers a lot of time, as they would only need to fill in the specific details of their protocol, rather than starting from scratch.

5. **Documentation and Tutorials:** Comprehensive documentation and step-by-step tutorials could make it easier for other developers to understand your work and build upon it. This could include explanations of how to use the "protocol_helper" library, how to implement a new consensus protocol, and how to use the testing and benchmarking tools.

6. **Automated Build and Testing:** Tools like continuous integration (CI) can automatically build your code and run tests whenever changes are made. This could help to catch bugs and issues early.

7. **Visualization Tools:** Visualization tools can help to understand the behavior of different consensus protocols. For example, you could create a tool that visualizes the blockchain as it grows, showing the creation of new blocks and the resolution of forks.

To further assist, I will take a look at the GitLab repository and the tutorial you've provided to get a more detailed understanding of your project.
