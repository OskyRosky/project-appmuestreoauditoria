# App Muestreo en Auditoria con LLM.

Este proyecto implementa una aplicación Shiny para realizar muestreo por unidades monetarias (MUM), una técnica de muestreo utilizada en auditorías para evaluar la importancia relativa de las transacciones y balances.

<table>
  <tr>
    <td><img src="/ima/Shinylogo.png" alt="LogoShiny" style="width: 150px;"/></td>
    <td><img src="/ima/rshiny.png" alt="LogoShiny2" style="width: 150px;"/></td>
  </tr>
</table>


 ![class](/ima/ima1.png)


---------------------------------------------

**Resumen del repositorio**

1.  **Intro** 🧳

Este repositorio contiene la aplicación Muestreo Auditoría, una herramienta diseñada para facilitar el análisis estadístico, la planificación de auditorías y la elaboración de informes automatizados mediante modelos de lenguaje (LLM). La App integra técnicas de muestreo financiero, análisis descriptivo, pruebas MUM, LES y atributos, y genera conclusiones inteligentes basadas en datos reales. El propósito principal es ofrecer una plataforma simple, práctica y robusta para apoyar el juicio profesional de auditoras y auditores en su trabajo diario.

2.  **Tech Stack** 🤖

La aplicación está construida sobre un conjunto sólido de tecnologías:

	•	R + Shiny como núcleo de la interfaz y procesamiento estadístico.
	•	Paquetería especializada para visualización, modelado y generación de reportes.
	•	rmarkdown y librerías de ofimática para la creación de documentos profesionales.
	•	httr2 como puente hacia un modelo LLM externo.
	•	Ollama como motor local de lenguaje natural para generar conclusiones automatizadas.
	•	Docker para empaquetar y distribuir la App de forma segura y reproducible.

Todo el entorno está optimizado para ejecutarse tanto en desarrollo como en producción.

3.  **Carcterísticas** 🤳🏽

El sistema ofrece un conjunto amplio de funcionalidades orientadas a la auditoría financiera:

	•	Carga de archivos y validación automática de estructuras.
	•	Análisis descriptivo con visualizaciones dinámicas.
	•	Módulos completos de muestreo: MUM, LES y Atributos.
	•	Generación automática de informes profesionales en formato Word.
	•	Redacción inteligente de conclusiones mediante un modelo LLM.
	•	Interfaz amigable, organizada en módulos claros y coherentes.
	•	Integración con recursos gráficos, imágenes, parámetros configurables y lógica personalizada.

Cada módulo está construido para guiar al usuario paso a paso.

4.  **Procesos** 👣

La App implementa un flujo estructurado que refleja el proceso real de una auditoría:

	1.	Carga y validación de datos.
	2.	Exploración estadística y análisis preliminar.
	3.	Selección del enfoque de muestreo según el objetivo.
	4.	Cálculo de parámetros, selección de muestras y evaluación.
	5.	Documentación sistemática de hallazgos.
	6.	Redacción automatizada del informe para agilizar la preparación de papeles de trabajo.

Todo se ejecuta dentro de una arquitectura modular que facilita el mantenimiento y la extensión.

5.  **Aprendizaje** 💡

La App implementa un flujo estructurado que refleja el proceso real de una auditoría:

	1.	Carga y validación de datos.
	2.	Exploración estadística y análisis preliminar.
	3.	Selección del enfoque de muestreo según el objetivo.
	4.	Cálculo de parámetros, selección de muestras y evaluación.
	5.	Documentación sistemática de hallazgos.
	6.	Redacción automatizada del informe para agilizar la preparación de papeles de trabajo.

Todo se ejecuta dentro de una arquitectura modular que facilita el mantenimiento y la extensión.

6.  **Mejoras** 🔩

Hay oportunidades para ampliar las capacidades del proyecto en futuras versiones:

	•	Añadir más modelos estadísticos y métodos de muestreo.
	•	Incorporar validaciones avanzadas para detectar anomalías en los datos.
	•	Optimizar la experiencia móvil y accesibilidad.
	•	Guardar configuraciones del usuario y permitir cargas sucesivas.
	•	Implementar un motor de reportes más flexible, con plantillas personalizables.
	•	Integrar almacenamiento externo para mantener históricos de análisis.

La base ya está sólida; ahora el enfoque puede dirigirse a enriquecer la experiencia.


7.  **Correr y dezplegar el proeycto** ⚙️

El repositorio incluye una estructura lista para desarrollo local y despliegue mediante contenedores. El proyecto puede ejecutarse tanto directamente desde RStudio como desde Docker, según las necesidades del entorno. La organización está pensada para que cualquier persona pueda levantar la App sin configuraciones complejas, gracias a scripts automatizados y variables de entorno claras.


8 .  **Aún más** 🙌🏽

Además de ser una herramienta técnica, este proyecto busca convertirse en un recurso práctico y educativo.
Su estructura modular permite que desarrolladores, auditores y estudiantes puedan aprender sobre muestreo, visualización de datos, automatización de informes y uso de modelos de lenguaje en el contexto de la auditoría.
La idea es que este repositorio siga creciendo, integrando mejoras y aportes de la comunidad que fortalezcan su utilidad y alcance.


---------------------------------------------

# :computer: App Muestreo en Auditoria con LLM  :computer:

---------------------------------------------

# 1. Overview del Proyecto 🧳

La App de Muestreo de Auditoría es una herramienta integral diseñada para apoyar a auditores, analistas financieros y equipos de fiscalización en la evaluación y validación de información numérica. Su objetivo es simplificar procesos complejos como el análisis descriptivo, la selección de muestras mediante metodologías reconocidas (MUM, LES y atributos) y la generación automatizada de conclusiones técnicas basadas en los datos cargados por el usuario.

La aplicación combina visualizaciones claras, análisis estadístico confiable y automatización inteligente para agilizar el trabajo de auditoría. Asimismo, incorpora un modelo de lenguaje (LLM) para redactar informes preliminares de forma precisa y contextualizada, lo que reduce tiempos y mejora la calidad del reporte final.

Está pensada para profesionales que buscan eficiencia, trazabilidad y consistencia en sus procedimientos, sin necesidad de depender de herramientas externas o procesos manuales.

# 2. Arquitectura y Tecnologías 🤖

La aplicación está construida sobre una arquitectura modular que facilita el mantenimiento, la escalabilidad y la claridad del proyecto. Cada parte cumple una función específica dentro del flujo de análisis y permite que la aplicación funcione de forma ordenada y confiable. En el núcleo se encuentra R + Shiny, responsables de la interfaz, la experiencia del usuario y la comunicación entre los módulos. La estructura se organiza en componentes independientes: la UI (header, sider y body), la lógica de servidor, los scripts de procesamiento y los recursos estáticos. Esta separación garantiza que cada parte pueda evolucionar sin afectar el resto del sistema.

Para la generación de documentos y la integración con modelos de lenguaje, la app incorpora rmarkdown, officer, flextable, y la comunicación directa con Ollama, que permite redactar informes automatizados con base en los datos y el contexto que se analiza.El despliegue está completamente containerizado mediante Docker, lo que asegura que la App funcione igual en cualquier entorno, sin depender de configuraciones externas. Dentro del contenedor se gestionan las dependencias, el entorno de ejecución y las librerías utilizadas, mientras que Ollama corre en el host y se comunica desde el contenedor a través del puente HTTP.

El resultado es una solución robusta, portable y reproducible, diseñada para que cualquier auditor o analista pueda ejecutarla sin fricciones.

# 3. Funcionalidades principales 📌

La aplicación reúne un conjunto de herramientas diseñadas específicamente para el análisis financiero y la elaboración de papeles de trabajo en auditoría. Cada módulo cumple una función práctica, orientada a facilitar el flujo de revisión, documentación y generación de evidencia.

Entre sus funcionalidades más importantes destacan:

## Análisis descriptivo inmediato

Permite cargar archivos, seleccionar variables y obtener estadísticas clave como mínimos, máximos, percentiles, promedios y medidas de dispersión. Incluye tablas y gráficos que ayudan a comprender la distribución y el comportamiento de los datos.

## Muestreo financiero especializado

La app integra diferentes enfoques utilizados en auditoría:

•	MUM (Monetary Unit Sampling)

•	LES (Line-Item Evaluation Sampling)

•	Muestreo por atributos

Cada módulo guía al usuario paso a paso para definir parámetros, generar muestras y visualizar la evidencia resultante.

## Generación automática de informes DOCX

En cada sección, el usuario puede generar un informe en formato Word.
Los reportes incluyen:

•	Los resultados del análisis realizado.

•	Gráficos y tablas relevantes.

•	Conclusiones redactadas en lenguaje técnico.

Todo se genera con plantillas consistentes y listas para incorporarse en el expediente de auditoría.

## Integración con Modelos de Lenguaje (LLM)

La aplicación se conecta con Ollama para redactar conclusiones, resúmenes y observaciones de auditoría basados en:

•	Los datos analizados.

•	El contexto proporcionado por el usuario.

•	Los resultados estadísticos de cada módulo.

Esto agiliza la documentación para los papeles de trabajo y aumenta la calidad de las conclusiones.

## Validación y limpieza automática

Cuando se cargan los datos, la app realiza verificaciones básicas:

•	Detección de tipos de variables.

•	Presencia de NA o valores extremos.

•	Compatibilidad de columnas según el módulo.

Estas validaciones reducen errores y aseguran un flujo continuo.

## Visualización clara y diseño moderno

La organización modular del UI (encabezado, menú lateral y cuerpo principal) permite navegar fácilmente por cada sección.
Se incluyen gráficos dinámicos, reactivos y paneles interactivos que facilitan el análisis visual.

# 4. Presentación visual de la App 🎨 (Screenshots)

![class](/ima/ima2.png)

![class](/ima/ima3.png)

![class](/ima/ima4.png)

![class](/ima/ima5.png)

![class](/ima/ima6.png)

![class](/ima/ima7.png)

# 5. Ejecución y Despliegue ⚙️


![ChatGPT](https://img.shields.io/badge/chatGPT-74aa9c?style=for-the-badge&logo=openai&logoColor=white)
![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Python](https://img.shields.io/badge/python-3670A0?style=for-the-badge&logo=python&logoColor=ffdd54)
![PyTorch](https://img.shields.io/badge/PyTorch-%23EE4C2C.svg?style=for-the-badge&logo=PyTorch&logoColor=white)
![Apache](https://img.shields.io/badge/apache-%23D42029.svg?style=for-the-badge&logo=apache&logoColor=white)


