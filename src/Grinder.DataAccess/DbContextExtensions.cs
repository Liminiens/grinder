using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;

namespace Grinder.DataAccess
{
    public static class DbContextExtensions
    {
        public static async Task AddOrUpdateUsers(this DbSet<User> dbSet, IEnumerable<User> records)
        {
            foreach (var data in records)
            {
                var fromDatabase = await dbSet.AsNoTracking().FirstOrDefaultAsync(x => x.Id == data.Id || x.UserId == data.UserId);
                if (fromDatabase != null)
                {
                    if (fromDatabase.Username != data.Username)
                    {
                        fromDatabase.Username = data.Username;
                        dbSet.Attach(fromDatabase).State = EntityState.Modified;
                    }
                }
                else
                {
                    dbSet.Attach(data).State = EntityState.Added;
                }
            }
        }
    }
}