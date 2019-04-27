using System.Collections.Generic;
using System.Linq;
using Microsoft.EntityFrameworkCore;

namespace Grinder.DataAccess
{
    public static class DbContextExtensions
    {
        public static void AddOrUpdateUsers(this DbSet<User> dbSet, IEnumerable<User> records)
        {
            foreach (var data in records)
            {
                var fromDatabase = dbSet.AsNoTracking().FirstOrDefault(x => x.Id == data.Id || x.UserId == data.UserId);
                if (fromDatabase != null && (fromDatabase.Username != data.Username))
                {
                    fromDatabase.Username = data.Username;
                    dbSet.Attach(fromDatabase).State = EntityState.Modified;
                }
                else
                {
                    dbSet.Attach(data).State = EntityState.Added;
                }
            }
        }
    }
}