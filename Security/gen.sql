CREATE SCHEMA [gen]
    AUTHORIZATION [dbo];




GO
EXECUTE sp_addextendedproperty @name = N'Description', @value = N'Генераторы объектов', @level0type = N'SCHEMA', @level0name = N'gen';

