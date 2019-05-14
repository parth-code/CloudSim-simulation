## **Simple simulation of datacenters using the CloudSim framework**

Consists of 2 models.

The 1st model is a simulation of IaaS(*Infrastructure as a Service*), where each VM is on a separate host.

The 2nd model is a simulation of SaaS(*Software as a Service*), spreadsheet program on a single VM, which is supplied with multiple cloudlets, which represent tasks performed on the spreadsheet.

The project required cloudsim jars to be added in libraries folder. 

Run using `sbt run`. 